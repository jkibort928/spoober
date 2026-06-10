import System.IO
import System.Environment (getArgs)
import System.Exit ( die )
import Control.Monad ( when, unless )
import Data.List ( intercalate, isInfixOf )
import Data.Char ( isSpace )

helpMessage :: String
helpMessage = "Usage: spoober [OPTIONS] <FILE> [MODULES]\n\nOPTIONS:\n\t-h: \t\tDisplay this help message\n\t-l: \t\tlist all modules within the file\n\t-m: \t\tOnly select packages within specified modules\n\t-e: \t\tExclude packages within specified modules\n\n\t--all:\t\tUncomment all conditional comments\t(*#, ?#, !#)\n\t--prospective: \tUncomment prospective packages\t\t(*#)\n\t--optional: \tUncomment optional packages \t\t(?#)\n\t--unneeded: \tUncomment unneeded packages \t\t(!#)\nFILE:\n\tThe infile to read\nMODULES:\n\tThe modules you wish to specify\n\t(will do nothing unless -m or -e is active)\n\nExamples:\n\tspoober -l infile.spoob\n\tspoober -m infile.spoob module1 module2\n\tspoober -e infile.spoob module3\n\tspoober infile.spoob --prospective --optional\n"

possibleFlags :: String
possibleFlags = "hlmen"
possibleLFlags :: [String]
possibleLFlags = ["help", "all", "prospective", "optional", "unneeded", "newline"]

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

isClosingHeader :: String -> Bool
isClosingHeader s = isHeader s && take 2 s == "</"

isOpeningHeader :: String -> Bool
isOpeningHeader s = isHeader s && not (isClosingHeader s)

-- "<h1>" and "</h1>" both become "h1"
normalizeHeader :: String -> String
normalizeHeader str = filter (`notElem` "<>/") str

processModules :: Bool -> [String] -> [String] -> [String]
processModules isExclude ms list = reverse (helper [] [] list)
    where
        helper acc modstack strs = case strs of
            [] -> acc
            (s:ss)
                | isOpeningHeader s -> case modstack of
                    (top:rest) | normalizeHeader s == top -> helper acc rest ss
                    _ -> helper acc ((normalizeHeader s):modstack) ss
                | isClosingHeader s -> case modstack of
                    []         -> errorWithoutStackTrace ("Unexpected closing header: " ++ s)
                    (top:rest) -> if normalizeHeader s == top 
                                  then helper acc rest ss 
                                  else errorWithoutStackTrace ("Mismatched header: expected </" ++ top ++ ">, got " ++ s)
                | shouldAccept modstack -> helper (s:acc) modstack ss
                | otherwise             -> helper acc     modstack ss

        shouldAccept stack = 
            let isActive = any (`elem` ms) stack
            in if isExclude then not isActive else isActive

-- Returns the strings that are included within the given modules
specifyModules :: [String] -> [String] -> [String]
specifyModules = processModules False

-- Returns the strings that are not included in the given modules
excludeModules :: [String] -> [String] -> [String]
excludeModules = processModules True

-- Deletes every line except for the headers, and removes the angle brackets
extractHeaders :: [String] -> [String]
extractHeaders []   = []
extractHeaders strs = map normalizeHeader (filter isHeader strs)

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
parseArgs []            = ([], "", [])
parseArgs strs = helper strs [] "" []
    where
        helper args argv flags longFlags = case args of
            (a:as)
                | isLongFlag a  -> helper as  argv            flags                   (longFlags ++ [remove2Dash a])
                | isFlag a      -> helper as  argv           (flags ++ removeDash a)   longFlags
                | otherwise     -> helper as (argv ++ [a])    flags                    longFlags
            []                  -> (argv, flags, longFlags)

-- Handles C-Style comments across the full text
handleMultilines :: String -> String
handleMultilines text = reverse (helper False [] text)
    where
        helper isComment acc str = case str of
            (c1:c2:cs)
                | isComment && [c1,c2] == "*/"  -> helper False acc cs
                | isComment                     -> helper True  acc (c2:cs)
                | [c1,c2] == "/*"               -> helper True  acc cs
                | otherwise                     -> helper False (c1:acc) (c2:cs) -- only iterate one character at a time, despite looking at 2
            (c:cs)
                | isComment                     -> helper True  acc cs
                | otherwise                     -> helper False (c:acc) cs
            []                                  -> acc

-- Handles Hash-Style comments across a single line
-- (treating # as a comment, and (*#, ?#, !#) as line-nulllifiers according to which longflags are specified)
handleComment :: LFlags -> String -> String
handleComment (fProspective, fOptional, fUnneeded) line
     | null stripped = ""
     | tagExists = if tagEnabled then makeClean remainder else ""
     | findError stripped =
         errorWithoutStackTrace ("Invalid comment: Optional tags must be at the start of the line: " ++ line)
     | otherwise = makeClean line
     where
         stripped    = dropWhile isSpace line
         tag         = take 2 stripped
         remainder   = drop 2 stripped
         
         tags        = ["*#", "?#", "!#"]
         tagExists   = tag `elem` tags
         makeClean   = takeWhile (/= '#')
         
         findError [] = False
         findError (c:cs)
             | c == '#' = False -- Terminate at true comment
             | take 2 (c:cs) `elem` tags = True
             | otherwise = findError cs

         tagEnabled = case tag of
             "*#" -> fProspective
             "?#" -> fOptional
             "!#" -> fUnneeded
             _    -> False

main :: IO ()
main = do
    args <- getArgs
    let (argv, flags, longFlags) = parseArgs args

    if ('h' `elem` flags) || ("help" `elem` longFlags) then do
        putStrLn helpMessage
    else do
        when (null argv)                $ die "Error: No arguments specified"
        unless (checkFlags flags)       $ die "Error: Invalid flag"
        unless (checkLFlags longFlags)  $ die "Error: Invalid long flag"
        when ('e' `elem` flags && 'm' `elem` flags) $ die "Error: Cannot use both -e and -m flags simultaneously"

        let (filePath:arguments) = argv
        
        {-
        print ("filePath: " ++ filePath)
        print ("arguments: " ++ concat arguments)
        print ("flags: " ++ flags)
        print ("longFlags: " ++ concat longFlags)
        -}
        
        rawText <- readFile filePath
        
        -- Determine longflags
        let isAll = "all" `elem` longFlags
            fProspective    = isAll || "prospective"    `elem` longFlags
            fOptional       = isAll || "optional"       `elem` longFlags
            fUnneeded       = isAll || "unneeded"       `elem` longFlags
            lf              = (fProspective, fOptional, fUnneeded)

        {- 
        Bottom to top logic: 
            1. Strip multilines
            2. Split to list of lines
            3. For each line (right to left logic):
                - Handle inline comments
                - Remove leading whitespace
                - Drop anything after trailing whitespace
            4. Purge empty strings from list
        -}
        let pkgList = deleteEmpty 
                    . map (takeWhile (not . isSpace) . dropWhile isSpace . handleComment lf)
                    . lines
                    $ handleMultilines rawText

        -- Separator for final output
        let sep = if ('n' `elem` flags || "newline" `elem` longFlags)
            then "\n"
            else " "
        
        if 'l' `elem` flags then do
                putStrLn (intercalate sep ((removeDups . extractHeaders) pkgList))
        else if 'e' `elem` flags then do
            putStrLn (intercalate sep (excludeModules arguments pkgList))
        else if 'm' `elem` flags then do
            putStrLn (intercalate sep (specifyModules arguments pkgList))
        else do
            putStrLn (intercalate sep (excludeModules [] pkgList))
