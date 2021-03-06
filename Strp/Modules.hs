-- |
-- Module      :  Strp.Modules
-- Copyright   :  2014 Joe Jevnik
-- License     :  GPL v3
--
-- Maintainer  :  Joe Jevnik
-- Stability   :  experimental
-- Portability :  requires xclip
--
-- Default modules for strp.

{-# LANGUAGE CPP,QuasiQuotes #-}

module Strp.Modules
    ( module Strp.Data
    , urlModule          -- :: StrpModule
    , filePathModule     -- :: StrpModule
    , searchEngineModule -- :: StrpModule
    ) where

import Strp.Data

import Control.Monad     (void,when)
import System.Directory  (doesDirectoryExist,doesFileExist)
import System.Process    (createProcess,CreateProcess(..),StdStream(..),proc)
import Text.RawString.QQ (r)
import Text.Regex.PCRE   ((=~))

-- | A 'StrpModule' for handling url data.
urlModule :: StrpModule
urlModule = StrpModule { strpMatch  = (=~
                                        [r|(?i)\b((?:https?:(?:/{1,3}|[a-z0-9%]\
)|[a-z0-9.\-]+[.](?:com|net|org|edu|gov|mil|aero|asia|biz|cat|coop|info|int|job\
s|mobi|museum|name|post|pro|tel|travel|xxx|ac|ad|ae|af|ag|ai|al|am|an|ao|aq|ar|\
as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|bi|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|ca|c\
c|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cs|cu|cv|cx|cy|cz|dd|de|dj|dk|dm|do|dz|ec|ee\
|eg|eh|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|\
gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|in|io|iq|ir|is|it|je|jm|jo|jp|ke|k\
g|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|me|mg|mh\
|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|mz|na|nc|ne|nf|ng|ni|nl|no|np|nr|\
nu|nz|om|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw|py|qa|re|ro|rs|ru|rw|sa|sb|sc|s\
d|se|sg|sh|si|sj|Ja|sk|sl|sm|sn|so|sr|ss|st|su|sv|sx|sy|sz|tc|td|tf|tg|th|tj|tk\
|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|\
yt|yu|za|zm|zw)/)(?:[^\s()<>{}\[\]]+|\([^\s()]*?\([^\s()]+\)[^\s()]*?\)|\([^\s]\
+?\))+(?:\([^\s()]*?\([^\s()]+\)[^\s()]*?\)|\([^\s]+?\)|[^\s`!()\[\]{};:'".,<>?\
«»“”‘’])|(?:(?<!@)[a-z0-9]+(?:[.\-][a-z0-9]+)*[.](?:com|net|org|edu|gov|mil|aer\
o|asia|biz|cat|coop|info|int|jobs|mobi|museum|name|post|pro|tel|travel|xxx|ac|a\
d|ae|af|ag|ai|al|am|an|ao|aq|ar|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|bi|bj|bm\
|bn|bo|br|bs|bt|bv|bw|by|bz|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cs|cu|cv|cx|\
cy|cz|dd|de|dj|dk|dm|do|dz|ec|ee|eg|eh|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|g\
e|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|im|in\
|io|iq|ir|is|it|je|jm|jo|jp|ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|\
ls|lt|lu|lv|ly|ma|mc|md|me|mg|mh|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|m\
z|na|nc|ne|nf|ng|ni|nl|no|np|nr|nu|nz|om|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw\
|py|qa|re|ro|rs|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|Ja|sk|sl|sm|sn|so|sr|ss|st|su|\
sv|sx|sy|sz|tc|td|tf|tg|th|tj|tk|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|us|uy|u\
z|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|yu|za|zm|zw)\b/?(?!@)))|]) -- "
                       , strpFunction = \cs -> void
                                        $ createProcess (proc "firefox" [cs])
                                          { std_err = CreatePipe }
                       }

-- | The module to handle local filepaths.
filePathModule :: StrpModule
filePathModule = StrpModule { strpMatch    = (=~ [r|^['"]?(?:/[^/]+)*['"]?$|])
                            , strpFunction = filePathModFunc
                            }

-- | The function to handle local filepaths.
filePathModFunc :: String -> IO ()
filePathModFunc cs = doesDirectoryExist cs
                     >>= \b -> when b (runFunc cs)
                     >>  doesFileExist cs
                     >>= \b -> when b (runFunc (f cs))
  where
      runFunc cs = void $ createProcess (proc "xterm" []) { cwd = Just cs }
      f       cs = reverse $ dropWhile (/= '/') $ reverse cs


-- | The module to search for a string in a search engine.
-- CATCH-ALL
searchEngineModule :: StrpModule
searchEngineModule =
    StrpModule { strpMatch    = const True
               , strpFunction = \cs -> void
                                $ createProcess (proc "firefox" [mkSearch cs])
                                      { std_err = CreatePipe }
               }
  where
      mkSearch cs = "www.google.com/#q="
                    ++ map (\c -> if c == ' ' then '+' else c) cs
                    ++ "&safe=off"
