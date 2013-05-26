import System.Environment
import System.Directory
import System.FilePath.Posix
import System.FilePath ((</>), addTrailingPathSeparator)
import Text.Regex.PCRE
import Control.Monad
import Data.List
import Control.Applicative

type Filename = FilePath
type Folder   = FilePath
type Filepath = FilePath

main :: IO ()
main = do
	putStrLn "digraph{"
	args  	<- getArgs
	results <- mapM (processDir . addTrailingPathSeparator) args -- :: IO [[(String, [String])]]
	let concatedResults = concat results -- :: [(String, [String])]
	mapM_ printDot concatedResults -- :: IO ()
	putStrLn "}"

createListDiccionary :: (a -> b) -> a -> (a, b)
createListDiccionary f item = (item, f item)

processDir :: Folder -> IO [(String, [String])]
processDir dir = do
	files <- getCoffeeFiles dir
	paths <- filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
	(mapM getDependencies files) -- ++ (mapM processDir paths)

getCoffeeFiles :: Folder -> IO [Filepath]
getCoffeeFiles path = do
	paths <- getDirectoryContents path
	let coffeeFilenames = filter isCoffeeFile paths
	return $ (map (path ++) coffeeFilenames)
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

printDot :: (String, [String]) -> IO ()
printDot (key, array) = mapM_ putStrLn $ map (\x -> mdl ++ (wrapInQuotes x)) array
	where 
		mdl = wrapInQuotes ++ " -> "

wrapInQuotes :: String -> String
wrapInQuotes str = "\"" ++ str ++ "\""