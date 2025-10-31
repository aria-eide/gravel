module Main (main) where

import Development.Shake
import Development.Shake.FilePath ((-<.>))

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles = buildDir, shakeThreads = 0}) do
  action (parseProjectFile >>= paths >>= need . snd)

  phony "clean" do
    putInfo "removing artifacts"
    liftIO (removeFiles "." artifacts)
    removeFilesAfter buildDir ["//*"]

  "//*.vo" %> \out -> do
    project <- parseProjectFile
    let source = out -<.> "v"
    Stdout depLine <-
      cmd (Traced "rocq dep") "rocq dep" (packageArgs project) source
    let deps = filter ("//*.vo" ?==) $ words $ dropWhile (/= ':') depLine
    need (source : deps)
    cmd_ (Traced "rocq compile") "rocq compile -q" (packageArgs project) source

projectFile, buildDir :: FilePath
projectFile = "_CoqProject"
buildDir = "_build"

artifacts :: [FilePath]
artifacts =
  [ "//*.glob",
    "//*.aux",
    "//*.vo",
    "//*.vos",
    "//*.vok",
    "//.lia.cache"
  ]

-- project file shenanigans ----------------------------------------------------

packageArgs :: Project -> [String]
packageArgs p =
  let one (Package c dir name) = [['-', c], dir, name]
   in concatMap one (packages p)

-- returns all source files and their '.vo' compiled names
paths :: Project -> Action ([FilePath], [FilePath])
paths p = do
  let inDir d = do
        names <- getDirectoryFiles d ["//*.v"]
        return (fmap ((d ++ "/") ++) names) -- rocq dislikes backslashes :(
  withinDirs <- concat <$> traverse inDir (directories p)
  let names = withinDirs ++ files p
  return (names, fmap (-<.> "vo") names)

data Project = Project
  { packages :: [Package],
    directories :: [FilePath],
    files :: [FilePath]
  }

-- A named package, as in '-Q theories Temporal'
data Package = Package Char FilePath String

parseProjectFile :: Action Project
parseProjectFile = do
  let collect [] = ([], [], [])
      collect ("-R" : ws) = package 'R' ws
      collect ("-Q" : ws) = package 'Q' ws
      collect (path : ws)
        | "//*.v" ?== path = ([], [], [path]) <> collect ws
        | otherwise = ([], [path], []) <> collect ws

      package _ [] = error "bad -R/-Q: missing directory name"
      package c (dir : ws) = theory c dir ws

      theory _ _ [] = error "bad -R/-Q: missing theory name"
      theory c dir (name : ws) = ([Package c dir name], [], []) <> collect ws

  p <- readFile' projectFile
  let (packages, directories, files) = collect (words p)
  return (Project {packages, directories, files})
