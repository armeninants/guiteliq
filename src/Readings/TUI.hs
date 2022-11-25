{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

{-
Module      : Readings.TUI
Description : Generic List Application
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Generic list application based on Brick TUI and abstract list interface.
-}

module Readings.TUI where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Core as BC
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Common.Config
import Data.List (find)
import qualified Data.Text as T
import Data.Tuple.Extra
import qualified Data.Vector as Vec
import Data.Version (showVersion)
import qualified Graphics.Vty as V
import Graphics.Vty.Input.Events hiding (Event)
import Lens.Micro.TH (makeLenses)
import Paths_guiteliq (version)
import RIO hiding (on)
import Readings.Core
import Readings.Model
import Utils.Brick
import Utils.Text

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

data ResourceName
  = ItemsList
  | Search
  deriving (Eq, Ord, Show)

data BrickState = BrickState
  { _config :: Config,
    _focusRing :: F.FocusRing ResourceName,
    -- _help :: Maybe (D.Dialog HelpChoice),
    _itemsList :: L.List ResourceName DocMetadata,
    _search :: E.Editor Text ResourceName,
    _allItems :: Vector DocMetadata
  }

makeLenses ''BrickState

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
        _allItems = l
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
drawUI s = case currentFocus of
  Just ItemsList -> [mainScreen]
  _anythingElse -> []
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
            getWidgetHorizontal globalActions
          ]

    searchBar = vLimit 1 $ E.renderEditor (txt . T.unlines) False (s ^. search)

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
  _ -> continue s
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
  V.EvKey key [] | key `elem` navKey -> do
    newItems <- L.handleListEvent e (s ^. itemsList)
    continue (s & itemsList .~ newItems)
  _other -> do
    newSearch <- E.handleEditorEvent e (s ^. search)
    continue (s & search .~ newSearch & filterResults)

navKey :: [V.Key]
navKey = [V.KUp, V.KDown, V.KHome, V.KEnd, V.KPageDown, V.KPageUp]

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
