-- Example main for strp

import Strp
import Strp.Modules

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= strp modules

-- | The list of modules you want to use for strp.
modules :: [StrpModule]
modules = [ urlModule ]
