{-# LANGUAGE OverloadedStrings #-}

module HaskellInDepth.Chapter6.Du.Main where

import qualified Data.Text.IO as TIO
import HaskellInDepth.Chapter6.Du.AppRWST (runMyApp)
import HaskellInDepth.Chapter6.Du.AppTypes (AppConfig)
import HaskellInDepth.Chapter6.Du.DirTree (dirTree, treeEntryBuilder)
import HaskellInDepth.Chapter6.Du.DiskUsage (diskUsage)
import HaskellInDepth.Chapter6.Du.FileCount (fileCount)
import System.Posix.Types (FileOffset)
import TextShow (Builder, TextShow, fromString, showb, toText, unlinesB)

work :: AppConfig -> IO ()
work config = do
  (_, dirs) <- runMyApp dirTree config ()
  (_, counters) <- runMyApp fileCount config ()
  (_, usages) <- runMyApp diskUsage config (0 :: FileOffset)
  let report =
        toText $
          buildEntries "Directory tree: " tabEntryBuilder dirs
            <> buildEntries "File counter: " tabEntryBuilder counters
            <> buildEntries "File space usage: " tabEntryBuilder usages
  TIO.putStr report

buildEntries :: Builder -> (e -> Builder) -> [e] -> Builder
buildEntries title entryBuilder entries = unlinesB $ title : map entryBuilder entries

tabEntryBuilder :: TextShow s => (FilePath, s) -> Builder
tabEntryBuilder (fp, s) = showb s <> "\t" <> fromString fp
