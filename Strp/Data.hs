-- |
-- Module      :  Strp.Data
-- Copyright   :  2014 Joe Jevnik
-- License     :  GPL v3
--
-- Maintainer  :  Joe Jevnik
-- Stability   :  experimental
-- Portability :  requires xclip
--
-- Data types used on strp.

module Strp.Data where

import Control.Applicative ((<$>))
import Data.List           (find)
import Text.Regex.PCRE     ((=~))

-- | A module for a pattern to catch and process.
data StrpModule = StrpModule { strpPattern  :: String -- ^ Regex 'String'.
                             , strpFunction :: String -> IO ()
                             }

instance Show StrpModule where
    show (StrpModule { strpPattern = p }) = p

-- | Searches a list of 'StrpModule's returning the first one if any
-- that match the source 'String'.
findModule :: String -> [StrpModule] -> Maybe StrpModule
findModule src = find ((=~) src . strpPattern)

-- | Processes a 'String' with a list of modules.
processString :: String -> [StrpModule] -> IO ()
processString src ms = case (strpFunction <$> (findModule src ms)) of
                           Nothing -> putStrLn $ "strp: " ++ src
                                      ++ ": no module used"
                           Just f  -> f src
