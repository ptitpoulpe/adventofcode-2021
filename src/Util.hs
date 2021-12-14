-- |
-- Various utility functions.
module Util where
import qualified Data.Text.Lazy as LT
import Debug.Trace (trace)
import Text.Pretty.Simple (pShow)
import System.IO.Unsafe (unsafePerformIO)

-- | get the (raw) input (as one line (with newlines in it))
inputRaw :: String -> String
inputRaw fileName = unsafePerformIO $ readFile fileName

pTraceShow :: Show a => a -> b -> b
pTraceShow x = trace (LT.unpack (pShow x))

pTraceShowId :: Show a => a -> a
pTraceShowId x = pTraceShow x x