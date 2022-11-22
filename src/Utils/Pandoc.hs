{-
Module      :  Utils.Pandoc
Description :  Pandoc Utilities
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

-}
module Utils.Pandoc where

import Data.List.Extra
import qualified Data.Text.IO as T
import RIO hiding (on)
import Text.Pandoc
import Text.Pandoc.Shared

-- ------------------------------------------

getTitle :: Pandoc -> Maybe Text
getTitle (Pandoc _meta blocks) = firstJust go blocks
  where
    go (Header _i _attr inline) = Just (stringify inline)
    go _ = Nothing

markdownToPandoc :: FilePath -> IO (Maybe Pandoc)
markdownToPandoc fp = do
  raw <- T.readFile fp
  rightToMaybe <$> runIO (readMarkdown def raw)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right b) = Just b