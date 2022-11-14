{-
Module      :  Utils.Markdown
Description :  Markdown Utilities
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

-}
module Utils.Markdown where

import RIO hiding (on)
import Utils.Pandoc

-- ------------------------------------------

getMarkdownTitle :: FilePath -> IO (Maybe Text)
getMarkdownTitle filePath = do
  mDoc <- markdownToPandoc filePath
  case mDoc of
    Nothing -> return Nothing
    Just doc -> case getTitle doc of
      Just title -> return $ Just title
      Nothing -> return $ Just "[No title]"
