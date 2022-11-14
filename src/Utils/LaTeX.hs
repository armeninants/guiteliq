{-# OPTIONS_GHC -Wno-orphans #-}

{-
Module      : Writings.Core
Description : Core of Writings App
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Core of Writings App
-}
module Utils.LaTeX where

import Control.Lens hiding (cons, view, (^.))
import Data.Data.Lens (uniplate)
import qualified Data.Text.IO as T
import RIO hiding (on)
import RIO.List (headMaybe)
import RIO.Text (pack)
import Text.LaTeX.Base.Parser (parseLaTeXFile)
import Text.LaTeX.Base.Pretty (prettyLaTeX)
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Syntax
  ( LaTeX (..),
    TeXArg (FixArg),
    lookForCommand,
  )

-- ------------------------------------------
-- LaTeX
-- ------------------------------------------

instance Plated LaTeX where
  plate = uniplate

getTeXTitle :: FilePath -> IO (Maybe Text)
getTeXTitle filePath = do
  res <- parseLaTeXFile filePath
  case res of
    Left _err -> return Nothing
    Right latex ->
      getTitleFromLaTeX latex
        & maybe "[No title]" prettyLaTeX
        & pack
        & Just
        & return

getTitleFromLaTeX :: LaTeX -> Maybe LaTeX
getTitleFromLaTeX latex = lookForCommand "title" latex & headMaybe >>= go
  where
    go [] = Nothing
    go ((FixArg arg) : _) = Just arg
    go (_ : rest) = go rest

onLaTeXFile :: FilePath -> (LaTeX -> LaTeX) -> IO ()
onLaTeXFile fpath f =
  parseLaTeXFile fpath
    >>= either (const $ return ()) (T.writeFile fpath . render . f)

setTitle :: Text -> LaTeX -> LaTeX
setTitle title = transform f
  where
    f (TeXComm "title" _args) = TeXComm "title" [FixArg (TeXRaw title)]
    f x = x
