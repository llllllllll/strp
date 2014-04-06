-- Example main for strp

import Strp
import Strp.Modules

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= strp modules

-- | The list of modules you want to use for strp.
-- NOTE: The order of the modules is the order they are checked.
modules :: [StrpModule]
modules = [ filePathModule
          , urlModule
          ]
