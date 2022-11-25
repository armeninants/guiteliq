{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

{-
Module      : Writings.TUI
Description : Generic List Application
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen.com
Stability   : experimental

Generic list application based on Brick TUI and abstract list interface.
-}

module Writings.TUI where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as BC
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Common.Config
import Conduit
import Data.List (find)
import qualified Data.Text as T
import Data.Tuple.Extra
import qualified Data.Vector as Vec
import Data.Version (showVersion)
import qualified Graphics.Vty as V
import Graphics.Vty.Input.Events hiding (Event)
import Lens.Micro ((?~))
import Lens.Micro.TH (makeLenses)
import Paths_guiteliq (version)
import RIO hiding (on)
import RIO.FilePath (isValid)
import RIO.List (intercalate)
import Utils.Brick
import Utils.Text
import Writings.Core
import Writings.Model

-- ------------------------------------------
-- Helpers
-- ------------------------------------------

hasKeyBinding :: Key -> [Modifier] -> [(a, (Key, [Modifier]), b)] -> Bool
hasKeyBinding key modifier = isJust . find ((== (key, modifier)) . snd3)

getAction :: Key -> [Modifier] -> [(a, (Key, [Modifier]), b)] -> Maybe b
getAction key modifier = fmap thd3 . find ((== (key, modifier)) . snd3)

getWidgetHorizontal :: [(Text, (Key, [Modifier]), b)] -> Widget ResourceName
getWidgetHorizontal = txt . mconcat . fmap go
  where
    go (label, (key, modifs), _) = " " <> modifsToText modifs <> keyToText key <> ":" <> label <> " "

-- ---------------------------------------------

type Event = ()

data NewItemAttr
  = NewItemTitle
  | NewItemName
  | NewItemTemplate
  deriving (Eq, Ord, Show)

data ResourceName
  = ItemsList
  | Search
  | NewItem !NewItemAttr
  deriving (Eq, Ord, Show)

data NewItemState = NewItemState
  { _nameState :: !(E.Editor Text ResourceName),
    _titleState :: !(E.Editor Text ResourceName),
    _templateState :: !(L.List ResourceName Text)
  }

data BrickState = BrickState
  { _config :: !Config,
    _focusRing :: !(F.FocusRing ResourceName),
    _itemsList :: !(L.List ResourceName DocMetadata),
    _search :: !(E.Editor Text ResourceName),
    _allItems :: !(Vector DocMetadata),
    _newItem :: !(Maybe NewItemState)
  }

makeLenses ''BrickState
makeLenses ''NewItemState

-- ---------------------------------------------

getEditorsValue :: E.Editor Text ResourceName -> Text
getEditorsValue = mconcat . E.getEditContents

getListValue :: L.List ResourceName Text -> Maybe (Int, Text)
getListValue = L.listSelectedElement

validateAttrsValues :: NewItemState -> Bool
validateAttrsValues NewItemState {..} =
  let validName = getEditorsValue _nameState & not . T.null
      validTitle = getEditorsValue _titleState & isValid . T.unpack
      validTemplate = getListValue _templateState & isJust
   in validName && validTitle && validTemplate

-- ---------------------------------------------

gqVersion :: Text
gqVersion = showVersion version & T.pack

initBrickState ::
  IO BrickState
initBrickState = do
  conf <- getConfig
  initAction & runRIO conf
  l <- getItems & runRIO conf
  pure
    BrickState
      { _config = conf,
        _focusRing = itemsListFocus,
        -- _help = Nothing,
        _itemsList = L.list ItemsList l 1,
        _search = E.editor Search Nothing "",
        _allItems = l,
        _newItem = Nothing
      }

itemsListFocus :: F.FocusRing ResourceName
itemsListFocus = F.focusRing [ItemsList]

runBrickApp ::
  IO BrickState
runBrickApp = initBrickState >>= defaultMain brickApp

brickApp ::
  M.App BrickState Event ResourceName
brickApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = pure,
      M.appAttrMap = theMap
    }

-- ------------------------------------------

renderAttrsWidgets ::
  NewItemState ->
  F.FocusRing ResourceName ->
  Widget ResourceName
renderAttrsWidgets NewItemState {..} fRing =
  intercalate [vLimit 1 (fill ' ')] ws & vBox
  where
    ws =
      [ [ txt "name",
          B.hBorder,
          vLimit 1 $
            E.renderEditor
              (txt . T.unlines)
              (F.focusGetCurrent fRing == Just (NewItem NewItemName))
              _nameState
        ],
        [ txt "title",
          B.hBorder,
          vLimit 1 $
            E.renderEditor
              (txt . T.unlines)
              (F.focusGetCurrent fRing == Just (NewItem NewItemTitle))
              _titleState
        ],
        [ txt "template",
          B.hBorder,
          L.renderList
            (const txt)
            (F.focusGetCurrent fRing == Just (NewItem NewItemTemplate))
            _templateState
        ]
      ]

-- ------------------------------------------
-- Widgets
-- ------------------------------------------

theMap :: brickState -> A.AttrMap
theMap _s =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.black),
      (L.listSelectedAttr, V.black `on` V.brightBlack),
      (L.listSelectedFocusedAttr, V.black `on` V.brightYellow),
      (E.editAttr, V.brightWhite `on` V.blue),
      (E.editFocusedAttr, V.black `on` V.yellow),
      (D.dialogAttr, V.brightWhite `on` V.blue),
      (D.buttonAttr, V.black `on` V.white),
      (D.buttonSelectedAttr, bg V.yellow)
    ]

drawUI ::
  BrickState ->
  [Widget ResourceName]
drawUI s = case (currentFocus, s ^. newItem) of
  (Just ItemsList, _) -> [mainScreen]
  (Just (NewItem _), Just nd) -> [drawNewItem nd (s ^. focusRing), mainScreen]
  _other -> []
  where
    currentFocus = F.focusGetCurrent (s ^. focusRing)

    mainScreen =
      if searchIsEmpty
        then itemsListWidget <=> statusBar
        else itemsListWidget <=> searchBar <=> statusBar

    searchIsEmpty = E.getEditContents (s ^. search) & T.concat & T.null

    itemsListWidget =
      withBorderStyle BS.unicodeBold $
        joinBorders
          . B.borderWithLabel (txt title)
          $ vBox [L.renderList drawDocumentItem True (s ^. itemsList)]

    title = " GUITELIQ " <> allCaps appName <> " " <> "v" <> gqVersion <> " "

    statusBar =
      vLimit 1 $
        hBox
          [ docNavWidget s,
            getWidgetHorizontal itemActions,
            getWidgetHorizontal globalActions,
            newItemWidget
          ]

    searchBar = vLimit 1 $ E.renderEditor (txt . T.unlines) False (s ^. search)
    newItemWidget = txt ("  Ctrl+N:New " <> itemName)

drawNewItem ::
  NewItemState ->
  F.FocusRing ResourceName ->
  Widget ResourceName
drawNewItem ns fRing =
  withBorderStyle BS.unicodeBold $
    C.centerLayer $
      B.borderWithLabel (txt $ " New " <> capitalise itemName <> " ") $
        padLeftRight 2 $
          padTopBottom 1 $
            hLimit 70 $
              vLimit 30 $
                vBox
                  [ renderAttrsWidgets ns fRing,
                    txt " ",
                    txt $
                      "[Esc]    - cancel.\n"
                        <> "[Tab]    - switch between editor and suggestions.\n"
                        <> "[Enter]  - create new "
                        <> itemName
                        <> ".\n"
                  ]

docNavWidget ::
  BrickState ->
  Widget ResourceName
docNavWidget s =
  txt "Item " <+> currentDocument <+> txt " of " <+> totalDocuments <+> txt " "
  where
    currentDocument = case s ^. itemsList . L.listSelectedL of
      Nothing -> txt "-"
      Just i -> str (show (i + 1))

    totalDocuments = str $ show $ Vec.length $ s ^. itemsList . L.listElementsL

drawDocumentItem ::
  Bool ->
  DocMetadata ->
  Widget ResourceName
drawDocumentItem _ model =
  BC.vLimit 1 $
    BC.hBox [txt (renderL model), txt " ", BC.fill ' ', txt (renderR model)]

-- ------------------------------------------
-- Event Handlers
-- ------------------------------------------

type AppEventM = EventM ResourceName (Next BrickState)

handleEvent ::
  BrickState ->
  BrickEvent ResourceName Event ->
  AppEventM
handleEvent s (VtyEvent e) = case (F.focusGetCurrent (s ^. focusRing), e) of
  (_, V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt s
  (r, V.EvKey V.KEsc []) | r /= Just ItemsList -> backToMain s
  (Just ItemsList, _) -> handleItemsListEvent s e
  (Just (NewItem _item), _) -> case s ^. newItem of
    Just nd -> handleNewItemScreenEvent s nd e
    Nothing -> continue s
  _other -> continue s
handleEvent s _ = continue s

cycleFocus ::
  BrickState ->
  AppEventM
cycleFocus s = continue $ s & focusRing %~ F.focusNext

backToMain ::
  BrickState ->
  AppEventM
backToMain s = continue $ s & focusRing .~ itemsListFocus

handleItemsListEvent ::
  BrickState ->
  V.Event ->
  AppEventM
handleItemsListEvent s e = case e of
  -- reset the search or exit the app
  V.EvKey V.KEsc [] -> do
    let filterText = T.strip . T.concat . E.getEditContents $ s ^. search
    if T.null filterText then halt s else continue $ setSearchField s ""
  V.EvKey key modifier | hasKeyBinding key modifier itemActions -> do
    let mAct = getAction key modifier itemActions
    let mItem = L.listSelectedElement (s ^. itemsList)
    case mItem of
      Nothing -> return ()
      Just (_, item) ->
        forM_ mAct $ runRIO (s ^. config) . ($ item)
    continue s
  V.EvKey key modifier | hasKeyBinding key modifier globalActions -> do
    let mAct = getAction key modifier globalActions
    forM_ mAct $ runRIO (s ^. config)
    continue s
  V.EvKey (V.KChar 'n') [V.MCtrl] -> beginNewDoc s
  V.EvKey key [] | key `elem` navKey -> do
    newItems <- L.handleListEvent e (s ^. itemsList)
    continue (s & itemsList .~ newItems)
  _ -> do
    newSearch <- E.handleEditorEvent e (s ^. search)
    continue (s & search .~ newSearch & filterResults)

navKey :: [V.Key]
navKey = [V.KUp, V.KDown, V.KHome, V.KEnd, V.KPageDown, V.KPageUp]

-- ------------------------------------------

handleNewItemScreenEvent ::
  BrickState ->
  NewItemState ->
  V.Event ->
  AppEventM
handleNewItemScreenEvent s nd@NewItemState {..} ev =
  let focus = F.focusGetCurrent (s ^. focusRing)
   in case (focus, ev) of
        (_, V.EvKey (V.KChar '\t') []) -> cycleFocus s
        (Just (NewItem _item), V.EvKey V.KEnter []) ->
          createNewItemEvent s nd
        (Just (NewItem NewItemName), _) -> do
          newItem' <- E.handleEditorEvent ev _nameState
          continue $ s & newItem . traverse . nameState .~ newItem'
        (Just (NewItem NewItemTitle), _) -> do
          newItem' <- E.handleEditorEvent ev _titleState
          continue $ s & newItem . traverse . titleState .~ newItem'
        (Just (NewItem NewItemTemplate), _) -> do
          newItem' <- L.handleListEvent ev _templateState
          continue $ s & newItem . traverse . templateState .~ newItem'
        _ -> continue s

-- ------------------------------------------

-- | if valid, then create a new item
createNewItemEvent ::
  BrickState ->
  NewItemState ->
  AppEventM
createNewItemEvent s nd@NewItemState {..} = do
  let conf = s ^. config
  if validateAttrsValues nd
    then do
      let title = getEditorsValue _titleState
          name = getEditorsValue _nameState
          template = getListValue _templateState
      mNewItem <- createNewDocument title name template & runRIO conf
      case mNewItem of
        Just newItm -> do
          newItemCallback newItm & runRIO conf
          let newVec = Vec.cons newItm (s ^. allItems)
          backToMain $ setAllItemsList s newVec
        Nothing -> backToMain s
    else continue s

initAttrsStates ::
  RIO Config NewItemState
initAttrsStates = do
  ts <- getTemplateDirs
  return $
    NewItemState
      { _nameState = E.editor (NewItem NewItemName) Nothing "",
        _titleState = E.editor (NewItem NewItemTitle) Nothing "",
        _templateState = L.list (NewItem NewItemTemplate) (Vec.fromList ts) 1
      }

beginNewDoc ::
  BrickState ->
  AppEventM
beginNewDoc s = do
  nd <- liftIO $ runRIO (s ^. config) initAttrsStates
  let newState =
        s
          & focusRing .~ F.focusRing (NewItem <$> [NewItemName, NewItemTitle, NewItemTemplate])
          & newItem ?~ nd
  handleNewItemScreenEvent newState nd (V.EvKey V.KDown [])

-- --------------------------------------
-- Helpers on the State
-- --------------------------------------

setAllItemsList ::
  BrickState ->
  Vector DocMetadata ->
  BrickState
setAllItemsList s l = filterResults (s & allItems .~ l)

setSearchField ::
  BrickState ->
  Text ->
  BrickState
setSearchField s t = s & search .~ E.editor Search Nothing t & filterResults

-- | Filter the results from hoogle using the search text
filterResults ::
  BrickState ->
  BrickState
filterResults st = st & itemsList .~ L.list ItemsList results 1
  where
    allResults = st ^. allItems
    filterText =
      T.toLower . T.strip . T.concat . E.getEditContents $ st ^. search
    results =
      if T.null filterText
        then allResults
        else Vec.filter (matchItem filterText) allResults