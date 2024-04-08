import System.IO
import System.Environment (getArgs)
import           Control.Exception
import Control.Monad ( when )

import Data.Typeable ( Typeable )
newtype Error = Error {errMsg :: String}
    deriving (Show, Typeable)
instance Exception Error

-- modified from https://stackoverflow.com/a/9722949
mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (a1, a2, a3) = (f a1, f a2, f a3)

isFlag :: String -> Bool
isFlag str = head str == '-'

removeDash :: String -> String
removeDash str = case str of
    (_:cs) -> cs

isLongFlag :: String -> Bool
isLongFlag str = case str of
    (c1:c2:cs) -> c1 == '-' && c2 == '-'
    _ -> False

remove2Dash :: String -> String
remove2Dash str = case str of
    (_:_:cs) -> cs


parseArgs :: [String] -> ([String], String, [String])
parseArgs []            = throw (Error "Error: No arguments specified")
parseArgs strs = helper strs [] "" []
helper :: [String] -> [String] -> String -> [String] -> ([String], String, [String])
helper args argv flags longFlags = case args of
    []                  -> (argv, flags, longFlags)
    (a:as)
        | isLongFlag a  -> helper as argv           flags                   (longFlags ++ [remove2Dash a])
        | isFlag a      -> helper as argv           (flags ++ removeDash a) longFlags
        | otherwise     -> helper as (argv ++ [a])  flags                   longFlags

--parseRaw :: String -> [String]


main :: IO ()
main = do
    args <- getArgs
    let (argv, flags, longFlags) = parseArgs args
    when (null argv) $ throw (Error "Error: No arguments specified")
    let (filePath:arguments) = argv
    print ("filePath: " ++ filePath)
    print ("arguments: " ++ concat arguments)
    print ("flags: " ++ flags)
    print ("longFlags: " ++ concat longFlags)
    rawFile <- readFile filePath
    putStrLn rawFile
