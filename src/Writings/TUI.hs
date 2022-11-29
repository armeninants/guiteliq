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
import Control.Lens ((%%~))
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
import Utils.Time

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
      M.appStartEvent = handleEvent (VtyEvent (EvKey KUp [])),
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
    (V.defAttr)
    [ (L.listAttr, fg V.brightWhite),
      (L.listSelectedAttr, fg V.brightWhite `V.withStyle` V.bold),
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
    BC.hBox
      [ modifyDefAttr (listAttrModif model) $ txt (model ^. docTitle), txt " "
      , BC.fill ' '
      , txt (formatYMD (model ^. docModificationTime))
      ]

listAttrModif :: DocMetadata -> V.Attr -> V.Attr
listAttrModif doc attr =
  let x = V.attrBackColor attr
      y = x == V.SetTo V.brightYellow
  in if y then attr else V.withForeColor attr fColor
  where
    fColor =
      case (doc ^. docType) of
        LaTeX -> V.brightWhite
        Markdown -> V.brightCyan

-- ------------------------------------------
-- Event Handlers
-- ------------------------------------------

type AppEventM = EventM ResourceName BrickState ()

handleEvent ::
  BrickEvent ResourceName Event ->
  AppEventM
handleEvent (VtyEvent e) = do
  s <- get
  case (F.focusGetCurrent (s ^. focusRing), e) of
    (_, V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
    (r, V.EvKey V.KEsc []) | r /= Just ItemsList -> backToMain s
    (Just ItemsList, _) -> handleItemsListEvent s e
    (Just (NewItem _item), _) -> case s ^. newItem of
      Just nd -> handleNewItemScreenEvent s nd e
      Nothing -> return ()
    _other -> return ()
handleEvent _ = return ()

cycleFocus ::
  BrickState ->
  AppEventM
cycleFocus s = put $ s & focusRing %~ F.focusNext

backToMain ::
  BrickState ->
  AppEventM
backToMain s = put $ s & focusRing .~ itemsListFocus

handleItemsListEvent ::
  BrickState ->
  V.Event ->
  AppEventM
handleItemsListEvent s e = case e of
  -- reset the search or exit the app
  V.EvKey V.KEsc [] -> do
    let filterText = T.strip . T.concat . E.getEditContents $ s ^. search
    if T.null filterText then halt else put $ setSearchField s ""
  V.EvKey key modifier | hasKeyBinding key modifier itemActions -> do
    let mAct = getAction key modifier itemActions
    let mItem = L.listSelectedElement (s ^. itemsList)
    case mItem of
      Nothing -> return ()
      Just (_, item) ->
        suspendAndResume $ forM_ mAct (runRIO (s ^. config) . ($ item)) >> return s
  V.EvKey key modifier | hasKeyBinding key modifier globalActions -> do
    let mAct = getAction key modifier globalActions
    suspendAndResume $ forM_ mAct (runRIO (s ^. config)) >> return s
  V.EvKey (V.KChar 'n') [V.MCtrl] -> beginNewDoc s
  V.EvKey key [] | key `elem` navKey -> do
    s' <- s & itemsList %%~ \w -> nestEventM' w (L.handleListEvent e)
    put s'
  _other -> do
    s' <- s & search %%~ \w -> nestEventM' w (E.handleEditorEvent (VtyEvent e))
    put $ filterResults s'

-- ------------------------------------------

handleNewItemScreenEvent ::
  BrickState ->
  NewItemState ->
  V.Event ->
  AppEventM
handleNewItemScreenEvent s nd ev =
  let focus = F.focusGetCurrent (s ^. focusRing)
   in case (focus, ev) of
        (_, V.EvKey (V.KChar '\t') []) -> cycleFocus s
        (Just (NewItem _item), V.EvKey V.KEnter []) ->
          createNewItemEvent s nd
        (Just (NewItem NewItemName), _) -> do
          s' <- s & newItem . traverse . nameState %%~ \w -> nestEventM' w (E.handleEditorEvent (VtyEvent ev))
          put s'
        (Just (NewItem NewItemTitle), _) -> do
          s' <- s & newItem . traverse . titleState %%~ \w -> nestEventM' w (E.handleEditorEvent (VtyEvent ev))
          put s'
        (Just (NewItem NewItemTemplate), _) -> do
          s' <- s & newItem . traverse . templateState %%~ \w -> nestEventM' w (L.handleListEvent ev)
          put s'
        _ -> return ()

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
    else return ()

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
  let newState = s
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
