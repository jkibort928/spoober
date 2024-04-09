import System.IO
import System.Environment (getArgs)
import Control.Exception ( throw, Exception )
import Control.Monad ( when, unless )
--import Data.Char (isSpace)

-- Error handling
import Data.Typeable ( Typeable )
newtype Error = Error {errMsg :: String}
    deriving (Show, Typeable)
instance Exception Error

{- Didnt need it afterall
-- modified from https://stackoverflow.com/a/9722949
mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (a1, a2, a3) = (f a1, f a2, f a3)
-}

{- unneeded too lol
-- modified from https://stackoverflow.com/a/20124026
isWhitespace :: String -> Bool
isWhitespace = all isSpace
-}

possibleFlags :: String
possibleFlags = "hlme"
possibleLFlags :: [String]
possibleLFlags = ["prospective", "optional", "unneeded"]

-- If the list is empty, return "", else it'll return the first element
firstOrEmpty :: [String] -> String
firstOrEmpty []         = ""
firstOrEmpty (str:strs) = str

-- Deletes all null strings from list
deleteEmpty :: [String] -> [String]
deleteEmpty = filter (not . null)

checkFlags :: [Char] -> Bool
checkFlags ""       = True
checkFlags (f:fs)   = f `elem` possibleFlags && checkFlags fs

checkLFlags :: [String] -> Bool
checkLFlags []          = True
checkLFlags (lf:lfs)    = lf `elem` possibleLFlags && checkLFlags lfs

isFlag :: String -> Bool
isFlag str = head str == '-'

removeDash :: String -> String
removeDash str = case str of
    (_:cs) -> cs

isLongFlag :: String -> Bool
isLongFlag str = case str of
    (c1:c2:cs) -> [c1, c2] == "--"
    _ -> False

remove2Dash :: String -> String
remove2Dash str = case str of
    (_:_:cs) -> cs

-- LFlag order: prospective, optional, unneeded
type LFlags = (Bool, Bool, Bool)

parseArgs :: [String] -> ([String], String, [String])
parseArgs []            = throw (Error "Error: No arguments specified")
parseArgs strs = helper strs [] "" []
    where
        helper args argv flags longFlags = case args of
            (a:as)
                | isLongFlag a  -> helper as  argv            flags                   (longFlags ++ [remove2Dash a])
                | isFlag a      -> helper as  argv           (flags ++ removeDash a)   longFlags
                | otherwise     -> helper as (argv ++ [a])    flags                    longFlags
            []                  -> (argv, flags, longFlags)


handleMultilines :: String -> String
handleMultilines text = reverse (helper False [] text)
    where
        helper isComment acc str = case str of
            (c1:c2:cs)
                | isComment && [c1,c2] == "*/"  -> helper False acc cs
                | isComment                     -> helper True acc (c2:cs)
                | [c1,c2] == "/*"               -> helper True acc cs
                | otherwise                     -> helper False (c1:acc) (c2:cs) -- only iterate one character at a time, despite looking at 2
            (c:cs)
                | isComment                     -> helper True acc cs
                | otherwise                     -> helper False (c:acc) cs
            []                                  -> acc

handleComments :: LFlags -> String -> String
handleComments lf [] = ""
handleComments (fProspective, fOptional, fUnneeded) line = reverse (helper [] line)
    where
        helper acc str = case str of
            (c1:c2:cs)
                | c1 == '#'         -> acc
                | [c1, c2] == "*#"  -> if fProspective then helper acc cs else acc
                | [c1, c2] == "?#"  -> if fOptional then helper acc cs else acc
                | [c1, c2] == "!#"  -> if fUnneeded then helper acc cs else acc
                | otherwise         -> helper (c1:acc) (c2:cs) -- iterate once despite looking at 2
            (c:cs)
                | c == '#'          -> acc
                | otherwise         -> helper (c:acc) cs
            []                      -> acc

parseRaw :: LFlags -> String -> [String]
parseRaw lf text = reverse (helper [] (lines text))
    where
        helper acc [] = acc
        helper acc (str:strs) = helper (handleComments lf str:acc) strs


main :: IO ()
main = do
    args <- getArgs
    let (argv, flags, longFlags) = parseArgs args

    when (null argv)                $ throw (Error "Error: No arguments specified")
    unless (checkFlags flags)       $ throw (Error "Error: Invalid flag")
    unless (checkLFlags longFlags)  $ throw (Error "Error: Invalid long flag")

    let (filePath:arguments) = argv
    
    {-
    print ("filePath: " ++ filePath)
    print ("arguments: " ++ concat arguments)
    print ("flags: " ++ flags)
    print ("longFlags: " ++ concat longFlags)
    -}
    
    rawText <- readFile filePath
    
    -- Handle multiline comments
    let parse1 = handleMultilines rawText

    -- Handle inline comments
    let lf = ("prospective" `elem` longFlags, "optional" `elem` longFlags, "unneeded" `elem` longFlags)
    let strList = parseRaw lf parse1

    -- Get rid of whitespace, and also limit each line to one word, dropping the rest
    let trimmedList = deleteEmpty (map (firstOrEmpty . words) strList)

    putStrLn (show trimmedList)
