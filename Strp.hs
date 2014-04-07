-- |
-- Module      :  Strp
-- Copyright   :  2014 Joe Jevnik
-- License     :  GPL v3
--
-- Maintainer  :  Joe Jevnik
-- Stability   :  experimental
-- Portability :  requires xclip
--
-- Invokation of Strp.hs

module Strp
    ( module Strp.Data
    , strp -- :: [StrpModule] -> [String] -> IO ()
    ) where

import Strp.Data

import System.Console.GetOpt
import System.Process        (readProcess)

-- | The command line argument flag types
data Flag = Version | Help | Clipboard

-- | The 'String' reflecting the version of this program.
versionString :: String
versionString =
    "strp: v0.0.0.1: 2014.4.6\n\
    \Copyright (C) 2014 Joe Jevnik\n\
    \This is free software; see the source for copying conditions.  There is NO\
    \\nwarranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR \
    \PURPOSE."

-- | Generates the help message from the options.
helpString :: [OptDescr a] -> String
helpString = (++) "Usage:" . usageInfo ""

-- | The possible command line options.
options :: [OptDescr Flag]
options = [ Option "v" ["version"]   (NoArg Version)
                       "Displays the current version number"
          , Option "h" ["help"]      (NoArg Help)
                       "Displays this message"
          , Option "c" ["clipboard"] (NoArg Clipboard)
                       "Reads the string out of the X primary clipboard"
          ]

-- | Parses the command line args.
handleFlags :: [StrpModule] -> ([Flag],[String],[String]) -> IO ()
handleFlags _ ([],[],_)       = putStrLn "Usage: strp h|v|c|STRING"
handleFlags ms (fs@(_:_),_,_) = mapM_ (handleFlag ms) fs
  where
      handleFlag _  Version   = putStrLn versionString
      handleFlag _  Help      = putStrLn $ helpString options
      handleFlag ms Clipboard = invoke ms Nothing
handleFlags ms ([],ss,_)      = mapM_ (invoke ms . Just) ss

-- | The main function for the strp program.
strp :: [StrpModule] -> [String] -> IO ()
strp ms = handleFlags ms . getOpt RequireOrder options

-- | Invokes strp with either a String or pulls one from xclip.
invoke :: [StrpModule] -> Maybe String -> IO ()
invoke ms Nothing   = readProcess "xclip" ["-o"] ""
                      >>= flip processString ms
invoke ms (Just cs) = processString cs ms
