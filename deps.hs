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
	putStrLn "digraph\n{"
	args <- getArgs
	folders <- mapM getFolders args -- [[Folder]]
	let folders' = map addTrailingPathSeparator $ concat  folders
	results <- mapM processDir folders' -- :: IO [[(String, [String])]]
	let concatedResults = concat results -- :: [(String, [String])]
	mapM_ printDependencies concatedResults -- :: IO ()
	putStrLn "}"

createListDiccionary :: (a -> b) -> a -> (a, b)
createListDiccionary f item = (item, f item)
		
processDir :: Folder -> IO [(String, [String])]
processDir dir = do
	files <- getCoffeeFiles dir
	mapM getDependencies files

getFolders :: Folder -> IO [FilePath]
getFolders path = do
	names <- getDirectoryContents path
	folders <- filterM isDirectory $ map (combine path) names
	folders' <- mapM getFolders folders
	return $ folders ++ (concat folders')

isDirectory :: Folder -> IO Bool
isDirectory d = do
    x <- doesDirectoryExist d
    return $  (x && (notElem (takeFileName d) [".",",", ".."]))

getCoffeeFiles :: Folder -> IO [Filepath]
getCoffeeFiles path = do
	names <- getDirectoryContents path
	let coffeeFilenames = filter isCoffeeFile names
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
		moduleFolder = last $ splitPath $ dropFileName $ dropExtension filepath
		moduleName = takeFileName filepath

printDependencies :: (String, [String]) -> IO ()
printDependencies (key, array) = mapM_ putStrLn $ map (getRelation key) array

getRelation :: String -> String -> String
getRelation a b = "\t" ++ (wrapInQuotes a) ++ " -> " ++ (wrapInQuotes b) ++ ";"

wrapInQuotes :: String -> String
wrapInQuotes str = "\"" ++ str ++ "\""