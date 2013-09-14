import System.Environment
import System.Directory
import System.FilePath.Posix
import Text.Regex.PCRE
import Control.Monad
import Control.Monad.Cont
import Data.List

main :: IO ()
main = do
  putStrLn "digraph\n{"
  args <- getArgs
  mapM_ (\x -> runCont (getFolders x) processDir) args
  putStrLn "}"

processDir :: FilePath -> IO ()
processDir dir = getDependencies dir >>= printDependencies

getFolders :: FilePath -> Cont (IO ()) FilePath
getFolders path = cont $ \fun -> do
	            names <- getDirectoryContents path
                    foldr (filterDir fun) (return ()) names
    where -- Se deberia poder mejorar los if, con alguna funcion que trate las monadas
      filterDir :: (String -> IO ()) -> FilePath -> IO () -> IO ()
      filterDir fun f xs = do dir <- isDirectory path'
                              if dir then runCont (getFolders path') fun
                              else
                                  if isCoffeeFile path' then fun path'
                                  else xs
          where path' = combine path f -- addTrailingPathSeparator

isDirectory :: FilePath -> IO Bool
isDirectory d = do
  x <- doesDirectoryExist d
  return $ (x && (notElem (takeFileName d) [".",",", ".."]))

isCoffeeFile :: FilePath -> Bool
isCoffeeFile filename = filename =~ ".coffee$" --  :: Bool

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
