{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

{-
Module      : UI.ListTUI
Description : Generic List Application
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Generic list application based on Brick TUI and abstract list interface.
-}

module UI.ListTUI where

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
import Conduit
import Data.List (find)
import qualified Data.Text as T
import Data.Tuple.Extra
import qualified Data.Vector as Vec
import Data.Version (showVersion)
import qualified Graphics.Vty as V
import Graphics.Vty.Input.Events hiding (Event)
import Interface.BrickAttribute
import Interface.ListInterface
import Lens.Micro ((?~))
import Lens.Micro.TH (makeLenses)
import Paths_guiteliq (version)
import RIO hiding (on)
import Utils.Brick
import Utils.Text

-- ------------------------------------------

type HasBrickListInterface conf itemModel newItemAttrs =
  ( HasListInterface conf itemModel newItemAttrs,
    All (BrickAttribute conf ResourceName) newItemAttrs,
    ApplyFuncToAttrs conf ResourceName newItemAttrs (RIO conf (Maybe itemModel))
  )

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

data HelpChoice = HelpClose

data ResourceName
  = Help
  | ItemsList
  | Search
  | NewItem Text
  deriving (Eq, Ord, Show)

instance BrickResource ResourceName where
  itemAttrResource = NewItem

data BrickState conf itemModel newItemAttrs = BrickState
  { _config :: conf,
    _focusRing :: F.FocusRing ResourceName,
    _help :: Maybe (D.Dialog HelpChoice),
    _itemsList :: L.List ResourceName itemModel,
    _search :: E.Editor Text ResourceName,
    _allItems :: Vector itemModel,
    _newItem :: Maybe (BrickAttrStateList conf ResourceName newItemAttrs)
  }

makeLenses ''BrickState

-- ---------------------------------------------

gqVersion :: Text
gqVersion = showVersion version & T.pack

initBrickState ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  IO (BrickState conf itemModel newItemAttrs)
initBrickState = do
  conf <- getConfig @conf
  initAction @conf & runRIO conf
  l <- getItems @conf & runRIO conf
  pure
    BrickState
      { _config = conf,
        _focusRing = itemsListFocus,
        _help = Nothing,
        _itemsList = L.list ItemsList l 1,
        _search = E.editor Search Nothing "",
        _allItems = l,
        _newItem = Nothing
      }

itemsListFocus :: F.FocusRing ResourceName
itemsListFocus = F.focusRing [ItemsList]

runBrickAppG ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  IO (BrickState conf itemModel newItemAttrs)
runBrickAppG = initBrickState >>= defaultMain brickApp

brickApp ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  M.App (BrickState conf itemModel newItemAttrs) Event ResourceName
brickApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = pure,
      M.appAttrMap = theMap
    }

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
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  BrickState conf itemModel newItemAttrs ->
  [Widget ResourceName]
drawUI s = case (currentFocus, s ^. newItem) of
  (Just Help, _) ->
    [helpScreen @conf (s ^. help), mainScreen]
  (Just ItemsList, _) -> [mainScreen]
  (Just (NewItem item), Just nd) | item `elem` getAttrsNames (attrsDescVector @conf) -> [drawNewItem s nd, mainScreen]
  _ -> []
  where
    currentFocus = F.focusGetCurrent (s ^. focusRing)

    mainScreen =
      if searchIsEmpty
        then itemsListWidget <=> statusBar
        else itemsListWidget <=> searchBar <=> statusBar

    searchIsEmpty = E.getEditContents (s ^. search) & T.concat & T.null

    itemsListWidget =
      withBorderStyle BS.unicodeBold
        $ joinBorders
          . B.borderWithLabel (txt title)
        $ vBox [L.renderList (drawDocumentItem @conf) True (s ^. itemsList)]

    title = " GUITELIQ " <> allCaps (appName @conf) <> " " <> "v" <> gqVersion <> " "

    statusBar =
      vLimit 1 $
        hBox
          [ docNavWidget s,
            str "  F1:Help",
            getWidgetHorizontal (itemActions @conf),
            getWidgetHorizontal (globalActions @conf),
            newItemWidget
          ]

    searchBar = vLimit 1 $ E.renderEditor (txt . T.unlines) False (s ^. search)
    newItemWidget =
      if hNull $ attrsDescVector @conf
        then txt ""
        else txt ("  Ctrl+N:New " <> itemName @conf)

drawNewItem ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  BrickState conf itemModel newItemAttrs ->
  BrickAttrStateList conf ResourceName newItemAttrs ->
  Widget ResourceName
drawNewItem s editors =
  withBorderStyle BS.unicodeBold $
    C.centerLayer $
      B.borderWithLabel (txt $ " New " <> capitalise (itemName @conf) <> " ") $
        padLeftRight 2 $
          padTopBottom 1 $
            hLimit 70 $
              vLimit 20 $
                vBox
                  [ renderAttrsWidgets editors (s ^. focusRing),
                    str " ",
                    txt $
                      "[Esc]    - cancel.\n"
                        <> "[Tab]    - switch between editor and suggestions.\n"
                        <> "[Enter]  - create new "
                        <> itemName @conf
                        <> ".\n"
                  ]

docNavWidget ::
  BrickState conf itemModel newItemAttrs ->
  Widget ResourceName
docNavWidget s =
  str "Item " <+> currentDocument <+> str " of " <+> totalDocuments
  where
    currentDocument = case s ^. itemsList . L.listSelectedL of
      Nothing -> str "-"
      Just i -> str (show (i + 1))

    totalDocuments = str $ show $ Vec.length $ s ^. itemsList . L.listElementsL

drawDocumentItem ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  Bool ->
  itemModel ->
  Widget ResourceName
drawDocumentItem _ model =
  BC.vLimit 1 $
    BC.hBox [txt (renderL @conf model), txt " ", BC.fill ' ', txt (renderR @conf model)]

-- ------------------------------------------
-- Event Handlers
-- ------------------------------------------

type AppEventM conf itemModel newItemAttrs = EventM ResourceName (Next (BrickState conf itemModel newItemAttrs))

handleEvent ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  BrickState conf itemModel newItemAttrs ->
  BrickEvent ResourceName Event ->
  AppEventM conf itemModel newItemAttrs
handleEvent s (VtyEvent e) = case (F.focusGetCurrent (s ^. focusRing), e) of
  (_, V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt s
  (r, V.EvKey V.KEsc []) | r /= Just ItemsList -> backToMain s
  (Just ItemsList, V.EvKey (V.KFun 1) []) -> openHelp s
  (Just Help, V.EvKey V.KEnter []) -> backToMain s
  (Just ItemsList, _) -> handleItemsListEvent s e
  (Just (NewItem item), _) | item `elem` getAttrsNames (attrsDescVector @conf) -> case s ^. newItem of
    Just nd -> handleNewItemScreenEvent s nd e
    Nothing -> continue s
  _ -> continue s
handleEvent s _ = continue s

cycleFocus ::
  BrickState conf itemModel newItemAttrs ->
  AppEventM conf itemModel newItemAttrs
cycleFocus s = continue $ s & focusRing %~ F.focusNext

openHelp ::
  BrickState conf itemModel newItemAttrs ->
  AppEventM conf itemModel newItemAttrs
openHelp s =
  continue $ s & focusRing .~ F.focusRing [Help] & help ?~ helpDialog

backToMain ::
  BrickState conf itemModel newItemAttrs ->
  AppEventM conf itemModel newItemAttrs
backToMain s = continue $ s & focusRing .~ itemsListFocus & help .~ Nothing

handleItemsListEvent ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  BrickState conf itemModel newItemAttrs ->
  V.Event ->
  AppEventM conf itemModel newItemAttrs
handleItemsListEvent s e = case e of
  -- reset the search or exit the app
  V.EvKey V.KEsc [] -> do
    let filterText = T.strip . T.concat . E.getEditContents $ s ^. search
    if T.null filterText then halt s else continue $ setSearchField s ""
  V.EvKey key modifier | hasKeyBinding key modifier (itemActions @conf) -> do
    let mAct = getAction key modifier (itemActions @conf)
    let mItem = L.listSelectedElement (s ^. itemsList)
    case mItem of
      Nothing -> return ()
      Just (_, item) -> do
        forM_ mAct $ runRIO (s ^. config) . ($ item)
    continue s
  V.EvKey key modifier | hasKeyBinding key modifier (globalActions @conf) -> do
    let mAct = getAction key modifier (globalActions @conf)
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
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  BrickState conf itemModel newItemAttrs ->
  BrickAttrStateList conf ResourceName newItemAttrs ->
  V.Event ->
  AppEventM conf itemModel newItemAttrs
handleNewItemScreenEvent s nd ev =
  let focus = F.focusGetCurrent (s ^. focusRing)
   in case (focus, ev) of
        (_, V.EvKey (V.KChar '\t') []) -> cycleFocus s
        (Just (NewItem item), V.EvKey V.KEnter []) | item `elem` getAttrsNames (attrsDescVector @conf) -> do
          createNewItemEvent s nd
        (Just (NewItem item), _) | item `elem` getAttrsNames (attrsDescVector @conf) -> do
          newItem' <- handleWidgetsEvents ev item nd
          continue $ s & newItem ?~ newItem'
        _ -> continue s

-- ------------------------------------------

-- | if valid, then create a new item
createNewItemEvent ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  BrickState conf itemModel newItemAttrs ->
  BrickAttrStateList conf ResourceName newItemAttrs ->
  AppEventM conf itemModel newItemAttrs
createNewItemEvent s nd = do
  let conf = s ^. config
  if validateAttrsValues nd
    then do
      mNewItem <- applyFuncToAttrs nd (createNewItem @conf) & runRIO conf
      case mNewItem of
        Just newItm -> do
          newItemCallback @conf newItm & runRIO conf
          let newVec = Vec.cons newItm (s ^. allItems)
          backToMain $ setAllItemsList s newVec
        Nothing -> backToMain s
    else continue s

beginNewDoc ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  BrickState conf itemModel newItemAttrs ->
  AppEventM conf itemModel newItemAttrs
beginNewDoc s =
  if hNull $ attrsDescVector @conf
    then continue s
    else do
      nd <- liftIO $ runRIO (s ^. config) $ initAttrsStates (attrsDescVector @conf)
      let newState =
            s
              & focusRing .~ F.focusRing (NewItem <$> getAttrsNames (attrsDescVector @conf))
              & newItem ?~ nd
      handleNewItemScreenEvent newState nd (V.EvKey V.KDown [])

-- ------------------------------------------
-- Help
-- ------------------------------------------

helpDialog :: D.Dialog HelpChoice
helpDialog = D.dialog (Just " Help ") (Just (0, choices)) 75
  where
    choices = [("Cool", HelpClose)]

helpScreen ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  Maybe (D.Dialog HelpChoice) ->
  Widget ResourceName
helpScreen (Just d) =
  withBorderStyle BS.unicodeBold $
    D.renderDialog d $
      C.hCenter $
        padAll 1 $
          vBox $
            map
              txt
              [ "Welcome to GUITELIQ!",
                "====================",
                " ",
                "[Enter] - open the " <> itemName @conf <> ".",
                " ",
                "[Esc] or [q] - quit from main screen.",
                "[Ctrl-c]     - quit from any screen.",
                "[Ctrl-t]     - open templates.",
                "[Ctrl-n]     - create a new " <> itemName @conf <> ".",
                "[F1]         - this help screen.",
                " ",
                "enjoy!"
              ]
helpScreen _ = str ""

-- --------------------------------------
-- Helpers on the State
-- --------------------------------------

setAllItemsList ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  BrickState conf itemModel newItemAttrs ->
  Vector itemModel ->
  BrickState conf itemModel newItemAttrs
setAllItemsList s l = filterResults (s & allItems .~ l)

setSearchField ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  BrickState conf itemModel newItemAttrs ->
  Text ->
  BrickState conf itemModel newItemAttrs
setSearchField s t = s & search .~ E.editor Search Nothing t & filterResults

-- | Filter the results from hoogle using the search text
filterResults ::
  forall conf itemModel newItemAttrs.
  HasBrickListInterface conf itemModel newItemAttrs =>
  BrickState conf itemModel newItemAttrs ->
  BrickState conf itemModel newItemAttrs
filterResults st = st & itemsList .~ L.list ItemsList results 1
  where
    allResults = st ^. allItems
    filterText =
      T.toLower . T.strip . T.concat . E.getEditContents $ st ^. search
    results =
      if T.null filterText
        then allResults
        else Vec.filter (matchItem @conf filterText) allResults
