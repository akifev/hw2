module Lib where

import Data.Maybe
import Data.List (dropWhileEnd, find, filter)
import Data.List.Split (splitOn)
import qualified Data.ByteString.Char8 as BSC
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory

import Report

data Node = Dir { dirChildren :: [Node]
                , dirPath :: FilePath
                , dirPermissions :: Permissions
                }
          | File { filePath :: FilePath
                 , fileText :: BSC.ByteString
                 , filePermissions :: Permissions
                 , fileType :: String
                 , fileCreationTime :: UTCTime
                 , fileModificationTime :: UTCTime
                 , isFileWatched :: Bool
                 , fileVersions :: [(Node, String)]
                 }

run :: FilePath -> IO ()
run path = do
  root <- pathToNode path
  commandHandler root root

pathToNode :: FilePath -> IO Node
pathToNode path = do
  permissions <- getPermissions path
  isFile <- doesFileExist path
  if isFile
  then do
    time <- getModificationTime path
    text <- BSC.readFile path
    return $ File { filePath = path
                  , fileText = text
                  , filePermissions = permissions
                  , fileType = getFileType path
                  , fileCreationTime = time
                  , fileModificationTime = time
                  , isFileWatched = False
                  , fileVersions = []
                  }
  else do
    children <- getChildren path
    return $ Dir { dirChildren = children
                 , dirPath = path
                 , dirPermissions = permissions
                 }

getChildren :: FilePath -> IO [Node]
getChildren "/" = do
  names <- listDirectory "/"
  let paths = map ((:) '/') names
  mapM pathToNode paths
getChildren path = do
  names <- listDirectory path
  let paths = map (\s -> path ++ ('/' : s)) names
  mapM pathToNode paths

getFileType :: FilePath -> String
getFileType path = dropWhile (/='.') $ getName path

getName :: FilePath -> String
getName path = reverse $ takeWhile (/='/') $ reverse path

-- COMMAND HANDLER

commandHandler :: Node -> Node -> IO ()
commandHandler root node@(Dir _ path _) = do
  putStr path
  putStr " > "
  line <- getLine
  runCommand root node (words line)

runCommand :: Node -> Node -> [String] -> IO ()
runCommand root node [] = commandHandler root node
runCommand root node ("cd" : xs) = cd root node xs
runCommand root node ("ls" : xs) = ls root node xs
runCommand root node ("dir" : xs) = dir root node xs
runCommand root node ("mkdir" : xs) = mkdir root node xs
runCommand root node ("touch" : xs) = touch root node xs
runCommand root node ("rm" : xs) = rm root node xs
runCommand root node ("search" : xs) = search root node xs
runCommand root node ("cat" : xs) = cat root node xs
runCommand root node ("write" : xs) = write root node xs
runCommand root node ("info" : xs) = info root node xs
runCommand root node ("help" : xs) = help root node xs
-- TODO VCS
runCommand root node ["exit"] = do
  clear root
  save root
runCommand root node _ = do
  reportUnknownCommand "Try: help"
  commandHandler root node

findNode :: Node -> FilePath -> IO (Maybe Node)
findNode root@(Dir children rootPath _) pathTo = do
  let rootPrefix = splitOn "/" rootPath
  let toDrop = length rootPrefix
  let (prefix, suffix) = splitAt toDrop $ splitOn "/" pathTo
  if prefix == rootPrefix
  then stepFindNode root root suffix
  else return Nothing

stepFindNode :: Node -> Node -> [String] -> IO (Maybe Node)
stepFindNode root File{} _ = return Nothing
stepFindNode _ node [] = return $ Just node
stepFindNode root (Dir children _ _) (to : xs@[]) = do
  let maybeNext = find (stepFindPredicate' to) children
  if isJust maybeNext
  then return $ Just (fromJust maybeNext)
  else return Nothing
stepFindNode root (Dir children _ _) (to : xs) = do
  let maybeNext = find (stepFindPredicate to) children
  if isJust maybeNext
  then stepFindNode root (fromJust maybeNext) xs
  else return Nothing

stepFindPredicate :: String -> Node -> Bool
stepFindPredicate to File{} = False
stepFindPredicate to (Dir children path _) | getName path == to = True
                                           | otherwise = False

stepFindPredicate' :: String -> Node -> Bool
stepFindPredicate' to (File path _ _ _ _ _ _ _) | getName path == to = True
                                                | otherwise = False
stepFindPredicate' to node = stepFindPredicate to node

addNode :: Node -> Node -> IO Node
addNode _ file@File{} = return file
addNode newNode@(Dir _ newNodePath _) node@(Dir children path permissions) =
  if path == newNodePath
  then return newNode
  else do
    childrenList <- mapM (addNode newNode) children
    return $ Dir { dirChildren = childrenList
                 , dirPath = path
                 , dirPermissions = permissions
                 }

cd :: Node -> Node -> [String] -> IO ()
cd root node [] = commandHandler root node
cd root node [path@('/':xs)] = do
  let newPath = cdHard "/" path
  tryCd root node newPath
cd root node@(Dir _ currentPath _) [path] = do
  let newPath = cdHard currentPath path
  tryCd root node newPath
cd root node _ = do
  reportCommandOptionsError "Too many arguments."
  commandHandler root node

tryCd :: Node -> Node -> FilePath -> IO ()
tryCd root node newPath = do
  maybeNode <- findNode root newPath
  if isJust maybeNode
  then do
    let nodeTo = fromJust maybeNode
    if getType nodeTo == "file"
    then do
      reportIncorrectPath "Expected path to directory, actual path to file."
      commandHandler root node
    else commandHandler root nodeTo
  else do
    reportIncorrectPath "Unknown path to directory."
    commandHandler root node

cdSimple :: FilePath -> FilePath -> FilePath
cdSimple cur "" = cur
cdSimple cur "." = cur
cdSimple cur ".." = let newPath = dropWhileEnd (/='/') cur in
                      if newPath == "/"
                      then "/"
                      else init newPath
cdSimple "/" to = '/' : to
cdSimple cur to = cur ++ ('/' : to)

cdHard :: FilePath -> FilePath -> FilePath
cdHard cur to = foldl cdSimple cur (splitOn "/" to)

ls :: Node -> Node -> [String] -> IO ()
ls root node [] = dir root node []
ls root node [path@('/':xs)] = do
  let newPath = cdHard "/" path
  tryLs root node newPath
ls root node@(Dir _ currentPath _) [path] = do
  let newPath = cdHard currentPath path
  tryLs root node newPath
ls root node _ = do
  reportCommandOptionsError "Too many arguments."
  commandHandler root node

tryLs :: Node -> Node -> FilePath -> IO ()
tryLs root node newPath = do
  maybeNode <- findNode root newPath
  if isNothing maybeNode
  then reportIncorrectPath "Unknown path to directory."
  else do
    let nodeTo = fromJust maybeNode
    if getType nodeTo == "file"
    then reportIncorrectPath "Expected path to directory, actual path to file."
    else showChildren $ map (getName . getPath) (getDirChildren nodeTo)
  commandHandler root node

dir :: Node -> Node -> [String] -> IO ()
dir root node@(Dir children _ _) [] = do
  showChildren $ map (getName . getPath) children
  commandHandler root node
dir root node _ = do
  reportCommandOptionsError "Too many arguments."
  commandHandler root node

getPath :: Node -> FilePath
getPath (Dir _ path _) = path
getPath (File path _ _ _ _ _ _ _) = path

getType :: Node -> String
getType File{} = "file"
getType Dir{} = "dir"

showChildren :: [FilePath] -> IO ()
showChildren [] = return ()
showChildren [x] = do
  putStr "└─ "
  putStrLn x
showChildren (x : xs) = do
  putStr "├─ "
  putStrLn x
  showChildren xs

mkdir :: Node -> Node -> [String] -> IO ()
mkdir root node [] = do
  reportCommandOptionsError "Not enouth arguments. Try: mkdir <path>"
  commandHandler root node
mkdir root node [path@('/':xs)] = do
  let newPath = cdHard "/" path
  tryAddNode root node newPath createChildDir
mkdir root node@(Dir _ currentPath _) [path] = do
  let newPath = cdHard currentPath path
  tryAddNode root node newPath createChildDir
mkdir root node _ = do
  reportCommandOptionsError "Too many arguments."
  commandHandler root node

tryAddNode :: Node -> Node -> FilePath -> (Node -> FilePath -> IO Node) -> IO ()
tryAddNode root node@(Dir _ currentPath _) newPath createChild = do
  maybeNode <- findNode root newPath
  if isJust maybeNode
  then do
    reportIncorrectPath "This path already exists."
    commandHandler root node
  else do
    let parentPath = cdHard newPath ".."
    maybeParentNode <- findNode root parentPath
    if isJust maybeParentNode
    then do
      let parentNode = fromJust maybeParentNode
      updatedParentNode <- createChild parentNode newPath
      newRoot <- addNode updatedParentNode root
      maybeCurrentNode <- findNode newRoot currentPath
      let currentNode = fromJust maybeCurrentNode
      commandHandler newRoot currentNode
    else do
      reportIncorrectPath $ "There aren't directory associated with path `" ++ parentPath ++ "`."
      commandHandler root node

tryRemoveNode :: Node -> Node -> FilePath -> IO ()
tryRemoveNode root node@(Dir _ currentPath _) newPath = do
  maybeNode <- findNode root newPath
  if isNothing maybeNode
  then do
    reportIncorrectPath $ "There aren't file or directory associated with path `" ++ newPath ++ "`."
    commandHandler root node
  else do
    let parentPath = cdHard newPath ".."
    maybeParentNode <- findNode root parentPath
    let parentNode = fromJust maybeParentNode
    updatedParentNode <- removeChild parentNode newPath
    newRoot <- addNode updatedParentNode root
    maybeCurrentNode <- findNode newRoot currentPath
    commandHandler newRoot $ fromMaybe newRoot maybeCurrentNode

tryUpdateNode :: Node -> Node -> FilePath -> BSC.ByteString -> Node -> IO ()
tryUpdateNode root node@(Dir _ currentPath _) newPath newText nodeTo = do
  maybeNode <- findNode root newPath
  if isNothing maybeNode
  then do
    reportIncorrectPath $ "There aren't file associated with path `" ++ newPath ++ "`."
    commandHandler root node
  else do
    let parentPath = cdHard newPath ".."
    maybeParentNode <- findNode root parentPath
    let parentNode = fromJust maybeParentNode
    updatedParentNode <- createChildWithText newText nodeTo parentNode newPath
    newRoot <- addNode updatedParentNode root
    maybeCurrentNode <- findNode newRoot currentPath
    let currentNode = fromJust maybeCurrentNode
    commandHandler newRoot currentNode

removeChild :: Node -> FilePath -> IO Node
removeChild (Dir children parentPath permissions) newPath =
  return $ Dir { dirChildren = filter (\s -> getPath s /= newPath) children
               , dirPath = parentPath
               , dirPermissions = permissions
               }

createChildDir :: Node -> FilePath -> IO Node
createChildDir (Dir children parentPath permissions) newPath = do
  let newChild = Dir { dirChildren = []
                     , dirPath = newPath
                     , dirPermissions = permissions
                     }
  return $ Dir { dirChildren = newChild : children
               , dirPath = parentPath
               , dirPermissions = permissions
               }

createChildFile :: Node -> FilePath -> IO Node
createChildFile (Dir children parentPath permissions) newPath = do
  time <- getCurrentTime
  let newChild = File { filePath = newPath
                      , fileText = BSC.empty
                      , filePermissions = permissions
                      , fileType = getFileType newPath
                      , fileCreationTime = time
                      , fileModificationTime = time
                      , isFileWatched = False
                      , fileVersions = []
                      }
  return $ Dir { dirChildren = newChild : children
               , dirPath = parentPath
               , dirPermissions = permissions
               }

touch :: Node -> Node -> [String] -> IO ()
touch root node [] = do
  reportCommandOptionsError "Not enouth arguments. Try: touch <path>"
  commandHandler root node
touch root node [path@('/':xs)] = do
  let newPath = cdHard "/" path
  tryAddNode root node newPath createChildFile
touch root node@(Dir _ currentPath _) [path] = do
  let newPath = cdHard currentPath path
  tryAddNode root node newPath createChildFile
touch root node _ = do
  reportCommandOptionsError "Too many arguments."
  commandHandler root node

rm :: Node -> Node -> [String] -> IO ()
rm root node [] = do
  reportCommandOptionsError "Not enouth arguments. Try: rm <path>"
  commandHandler root node
rm root node [path@('/':xs)] = do
  let newPath = cdHard "/" path
  tryRemoveNode root node newPath
rm root node@(Dir _ currentPath _) [path] = do
  let newPath = cdHard currentPath path
  tryRemoveNode root node newPath
rm root node _ = do
  reportCommandOptionsError "Too many arguments."
  commandHandler root node

search :: Node -> Node -> [String] -> IO ()
search root node [] = do
  reportCommandOptionsError "Not enouth arguments. Try: search <file-name>"
  commandHandler root node
search root node [name] = do
  searchRec node name
  commandHandler root node
search root node _ = do
  reportCommandOptionsError "Too many arguments."
  commandHandler root node

searchRec :: Node -> String -> IO ()
searchRec node@(File path _ _ _ _ _ _ _) name | getName path == name = putStrLn path
                                              | otherwise = return ()
searchRec node@(Dir children _ _) name = searchChildren children name

searchChildren :: [Node] -> String -> IO ()
searchChildren [] name = return ()
searchChildren (child : xs) name = do
  searchRec child name
  searchChildren xs name

cat :: Node -> Node -> [String] -> IO ()
cat root node [] = do
  reportCommandOptionsError "Not enouth arguments. Try: search <file-name>"
  commandHandler root node
cat root node@(Dir _ currentPath _) [path@('/':xs)] = do
  let newPath = cdHard "/" path
  tryCat root node newPath
cat root node@(Dir _ currentPath _) [path] = do
  let newPath = cdHard currentPath path
  tryCat root node newPath
cat root node _ = do
  reportCommandOptionsError "Too many arguments."
  commandHandler root node

tryCat :: Node -> Node -> FilePath -> IO ()
tryCat root node newPath = do
  maybeNode <- findNode root newPath
  if isJust maybeNode
  then do
    let nodeTo = fromJust maybeNode
    if getType nodeTo == "dir"
    then reportIncorrectPath "Expected path to file, actual path to directory."
    else BSC.putStrLn $ getFileText nodeTo
    commandHandler root node
  else do
    reportIncorrectPath "Unknown path to directory."
    commandHandler root node

getFileText :: Node -> BSC.ByteString
getFileText (File _ text _ _ _ _ _ _) = text
getFileText _ = BSC.empty

getDirChildren :: Node -> [Node]
getDirChildren (Dir children _ _) = children
getDirChildren _ = []


write :: Node -> Node -> [String] -> IO ()
write root node [] = do
  reportCommandOptionsError "Not enouth arguments. Try: write <path> <text>"
  commandHandler root node
write root node [_] = commandHandler root node
write root node@(Dir _ currentPath _) (path@('/':xs) : [text]) = do
  let newPath = cdHard "/" path
  tryWrite root node newPath text
write root node@(Dir _ currentPath _) (path : [text]) = do
  let newPath = cdHard currentPath path
  tryWrite root node newPath text
write root node _ = do
  reportCommandOptionsError "Too many arguments."
  commandHandler root node

tryWrite :: Node -> Node -> FilePath -> String -> IO ()
tryWrite root node newPath text = do
  maybeNode <- findNode root newPath
  if isJust maybeNode
  then do
    let nodeTo = fromJust maybeNode
    if getType nodeTo == "dir"
    then reportIncorrectPath "Expected path to file, actual path to directory."
    else do
      let newText = BSC.append (getFileText nodeTo) (BSC.pack text)
      tryUpdateNode root node newPath newText nodeTo
    commandHandler root node
  else do
    reportIncorrectPath "Unknown path to directory."
    commandHandler root node

createChildWithText :: BSC.ByteString -> Node -> Node -> FilePath -> IO Node
createChildWithText text nodeTo (Dir children parentPath permissions) newPath = do
  time <- getCurrentTime
  let newChild = File { filePath = newPath
                      , fileText = text
                      , filePermissions = permissions
                      , fileType = getFileType newPath
                      , fileCreationTime = time
                      , fileModificationTime = time
                      , isFileWatched = getIsWatched nodeTo
                      , fileVersions = getVersions nodeTo
                      }
  return $ Dir { dirChildren = newChild : children
               , dirPath = parentPath
               , dirPermissions = permissions
               }

getIsWatched :: Node -> Bool
getIsWatched (File _ _ _ _ _ _ flag _) = flag
getIsWatched _ = False

getVersions :: Node -> [(Node, String)]
getVersions (File _ _ _ _ _ _ _ v) = v
getVersions _ = []

info :: Node -> Node -> [String] -> IO ()
info root node [] = info root node ["."]
info root node [path@('/':xs)] = do
  let newPath = cdHard "/" path
  tryInfo root node newPath
info root node@(Dir _ currentPath _) [path] = do
  let newPath = cdHard currentPath path
  tryInfo root node newPath
info root node _ = do
  reportCommandOptionsError "Too many arguments."
  commandHandler root node

tryInfo :: Node -> Node -> FilePath -> IO ()
tryInfo root node newPath = do
  maybeNode <- findNode root newPath
  maybe (reportIncorrectPath "Unknown path to directory.") showInfo maybeNode
  commandHandler root node

showInfo :: Node -> IO ()
showInfo (File path text permissions typo creationTime modificationTime _ _) = do
  putStr "Path: "
  putStrLn path
  putStr "Permissions: "
  print permissions
  putStr "Type: "
  putStrLn typo
  putStr "Creation time: "
  print creationTime
  putStr "Modification time: "
  print modificationTime
  putStr "Size: "
  print $ BSC.length text
showInfo node@(Dir children path permissions) = do
  putStr "Path: "
  putStrLn path
  putStr "Permissions: "
  print permissions
  let (size, files) = processDirChildren children
  putStr "Size: "
  print size
  putStr "Files: "
  print files

processDirChildren :: [Node] -> (Int, Int)
processDirChildren [] = (0, 0)
processDirChildren (File _ text _ _ _ _ _ _ : xs) =
  let (w, c) = processDirChildren xs
   in (BSC.length text + w, 1 + c)
processDirChildren (Dir children _ _ : xs) =
  let (w, c) = processDirChildren xs
      (innerW, innerC) = processDirChildren children
   in (innerW + w, innerC + c)

help :: Node -> Node -> [String] -> IO ()
help root node _ = do
  putStrLn "cd <path> - переходит в указанную директорию;"
  putStrLn "ls <path> - содержимое указанной директории;"
  putStrLn "dir - содержимое текущей директории;"
  putStrLn "mkdir <path> - создаёт новую директорию;"
  putStrLn "touch <path> - создаёт новый файл;"
  putStrLn "cat <path> - выводит содержимое файла;"
  putStrLn "rm <path> - удаляет файл или директорию;"
  putStrLn "write <path> IN_ONE_WORD_TEXT - дописывает в файл;"
  putStrLn "search <name> - ищет вхождения по имени файла;"
  putStrLn "info <path> -выводит информацию о файле или директории;"
  putStrLn "exit - выходит и записывает состояние;"
  commandHandler root node

-- TODO VCS

clear :: Node -> IO ()
clear (Dir _ path _) = removeDirectoryRecursive path

save :: Node -> IO ()
save (Dir children path _) = do
  createDirectoryIfMissing True path
  saveChildren children
save (File path text _ _ _ _ _ _) = BSC.writeFile path text

saveChildren :: [Node] -> IO ()
saveChildren [] = return ()
saveChildren (x : xs) = do
  save x
  saveChildren xs
