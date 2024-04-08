import System.IO
import System.Environment (getArgs)
import           Control.Exception
import           Data.Typeable
newtype Error = Error {errMsg :: String}
    deriving (Show, Typeable)
instance Exception Error


parseArgs :: [String] -> (String, [String])
parseArgs (str:strs)  = (str, strs)
parseArgs []          = throw (Error "Error: No infile specified")

main :: IO ()
main = do
    args <- getArgs
    let (filePath, argv) = parseArgs args
    rawFile <- readFile filePath
    putStrLn rawFile
