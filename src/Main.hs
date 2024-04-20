import System.IO
import System.Environment (getArgs)
import Control.Exception ( throw, Exception )
import Control.Monad ( when, unless )
--import Data.Char (isSpace)
import Data.List ( intercalate )

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

helpMessage :: String
helpMessage = "hai"

listToPrint :: [String] -> String
listToPrint = unwords

possibleFlags :: String
possibleFlags = "hlme"
possibleLFlags :: [String]
possibleLFlags = ["all", "prospective", "optional", "unneeded"]

-- If the list is empty, return "", else it'll return the first element
firstOrEmpty :: [String] -> String
firstOrEmpty []         = ""
firstOrEmpty (str:strs) = str

-- Deletes all null strings from list
deleteEmpty :: [String] -> [String]
deleteEmpty = filter (not . null)

-- Removes duplicate entries from a list
removeDups :: forall a. Eq a => [a] -> [a]
removeDups []       = []
removeDups (x:xs)   = x : removeDups (filter (/= x) xs)

isHeader :: String -> Bool
isHeader [] = False
isHeader (c:_) = c == '<'

-- Filters all headers out of the list
filterAllHeaders :: [String] -> [String]
filterAllHeaders = filter (not . isHeader)

-- Trims one element off the left of a list
trimL :: [a] -> [a]
trimL []        = []
trimL (x:xs)    = xs

-- Trims one element off the right of a list 
trimR :: [a] -> [a]
trimR l = reverse (helper [] l)
   where
      helper acc lis = case lis of
         (x:[])    -> acc
         (x:xs)    -> helper (x:acc) xs
         otherwise -> acc

-- Trims the left and right elements off a list 
trimLR :: [a] -> [a]
trimLR = trimR . trimL

-- Returns the strings that are included within the given modules
specifyModules :: [String] -> [String] -> [String]
specifyModules mods list = reverse (helper [] "" mods list)
    where
        helper acc currmod ms strs 
            | currmod == "" = case strs of
                (s:ss)
                    | isHeader s && trimLR s `elem` ms    -> helper acc (trimLR s) ms ss  -- Feasible header found
                    | otherwise                             -> helper acc ""         ms ss  -- Accept nothing until we find a feasible header
                [] -> acc
            | otherwise     = case strs of
                (s:ss)
                    | isHeader s && trimLR s == currmod   -> helper acc     ""      ms ss -- We hit the end of the module
                    | isHeader s                            -> helper acc     currmod ms ss -- Skip headers
                    | otherwise                             -> helper (s:acc) currmod ms ss -- Accept the string
                [] -> acc

-- Returns the strings that are not included in the given modules
excludeModules :: [String] -> [String] -> [String]
excludeModules mods list = reverse (helper [] "" mods list)
    where
        helper acc currmod ms strs
            | currmod == "" = case strs of
                (s:ss)
                    | isHeader s && trimLR s `elem` ms    -> helper acc     (trimLR s) ms ss -- Infeasible header found
                    | isHeader s                            -> helper acc     ""         ms ss -- Skip headers
                    | otherwise                             -> helper (s:acc) ""         ms ss -- Accept the string
                [] -> acc
            | otherwise     = case strs of
                (s:ss)
                    | isHeader s && trimLR s == currmod   -> helper acc ""      ms ss -- We hit the end of the module
                    | otherwise                             -> helper acc currmod ms ss -- Accept nothing until we hit the end of the module
                [] -> acc

-- Deletes every line except for the headers, and removes the angle brackets
extractHeaders :: [String] -> [String]
extractHeaders []   = []
extractHeaders strs = map trimLR (filter isHeader strs)

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

    if 'h' `elem` flags then do
        putStrLn helpMessage
    else do
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
        
        let lf =
                if "all" `elem` longFlags then
                    (True, True, True)
                else
                    ("prospective" `elem` longFlags, "optional" `elem` longFlags, "unneeded" `elem` longFlags)
                    
        let strList = parseRaw lf parse1

        -- Get rid of whitespace, and also limit each line to one word, dropping the rest
        let trimmedList = deleteEmpty (map (firstOrEmpty . words) strList)
        
        if 'l' `elem` flags then do
                putStrLn (listToPrint ((removeDups . extractHeaders) trimmedList))
        else if 'e' `elem` flags then do
            putStrLn (listToPrint (excludeModules arguments trimmedList))
        else if 'm' `elem` flags then do
            putStrLn (listToPrint (specifyModules arguments trimmedList))
        else do
            let finalList = filterAllHeaders trimmedList
            putStrLn (listToPrint finalList)
