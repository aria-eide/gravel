module Main (main) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Development.Shake
import Development.Shake.FilePath ((-<.>))
import System.IO qualified as IO

projectFile, buildDir :: FilePath
projectFile = "_CoqProject"
buildDir = "_build"

main :: IO ()
main = do
  project <- parseProjectFile
  graph <- parseDeps . fromStdout <$> cmd "rocq dep -f" projectFile
  putStrLn "(got dependency graph)"

  shakeArgs (shakeOptions {shakeFiles = buildDir, shakeThreads = 0}) do
    want (Map.keys graph)

    phony "clean" do
      putInfo "removing artifacts"
      liftIO (removeFiles "." artifacts)
      removeFilesAfter buildDir ["//*"]

    "//*.vo" %> \out -> do
      let src = out -<.> "v"
      need (src : fromJust (Map.lookup out graph))
      cmd_ (Traced "rocq compile") "rocq compile -q" (packageArgs project) src

parseDeps :: String -> Map FilePath [FilePath]
parseDeps = foldMap one . lines
  where
    one line =
      let (lhs, rhs) = span (/= ':') line
          lhs' = filter relevant (words lhs)
          rhs' = filter relevant (words (drop 1 rhs))
       in Map.fromList (map (,rhs') lhs')

    relevant path = ("//*.vo" ?== path) || ("//*.v" ?== path)

packageArgs :: Project -> [String]
packageArgs p =
  let one (Package c dir name) = [['-', c], dir, name]
   in concatMap one (packages p)

data Project = Project {packages :: [Package]}

-- A named package, as in '-Q theories Temporal'
data Package = Package Char FilePath String

parseProjectFile :: IO Project
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

  p <- IO.readFile' projectFile
  let (packages, _directories, _files) = collect (words p)
  return (Project {packages})

artifacts :: [FilePath]
artifacts =
  [ "//*.glob",
    "//*.aux",
    "//*.vo",
    "//*.vos",
    "//*.vok",
    "//.lia.cache"
  ]
