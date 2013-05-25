import System.Environment
import System.Directory
import System.FilePath.Posix
import Text.Regex.PCRE
import Control.Monad
import Data.List

type Filename = FilePath
type Folder   = FilePath
type Filepath = FilePath

main :: IO ()
main = do
	args <- getArgs
	results <- mapM processDir args -- :: IO [[(String, [String])]]
	let concatedResults = concat results -- :: [(String, [String])]
	mapM_ printDependencies concatedResults -- :: IO ()

createListDiccionary :: (a -> b) -> a -> (a, b)
createListDiccionary f item = (item, f item)
		
processDir :: Folder -> IO [(String, [String])]
processDir dir = do
	files <- getCoffeeFiles dir
	mapM getDependencies files >>= return

getCoffeeFiles :: Folder -> IO [Filepath]
getCoffeeFiles path = do
	filenames <- getDirectoryContents path
	let coffeeFilenames = filter isCoffeeFile filenames
	return $ map (path ++) coffeeFilenames
	where
		isCoffeeFile :: Filename -> Bool
		isCoffeeFile filename = filename =~ ".coffee$" :: Bool

getDependencies :: FilePath -> IO (String, [String])
getDependencies filepath = do
	dat <- readFile filepath
	let matches = dat =~ pattern :: [[String]]
	return (moduleFolder ++ moduleName, map last matches)
	where 
		pattern = "require \"([A-z/.]+)\""
		moduleFolder = last $ splitPath $dropFileName $ dropExtension filepath
		moduleName = takeFileName filepath

printDependencies :: (String, [String]) -> IO ()
printDependencies (key, array) = putStrLn (key ++ " -> " ++ (intercalate ", " array))