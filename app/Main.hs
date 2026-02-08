module Main (main) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Development.Shake
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.FilePath ((-<.>))
import GHC.Generics (Generic)

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

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles = buildDir, shakeThreads = 0}) do
  projectDeps <- addOracle \(ProjectDeps ()) -> do
    need [projectFile]
    parseDeps . fromStdout
      <$> cmd (Traced "rocq dep") "rocq dep -f" [projectFile]

  fileDeps <- addOracle \(FileDeps file) -> do
    graph <- projectDeps (ProjectDeps ())
    return (fromJust (Map.lookup file graph))

  project <- addOracle \(ProjectFile ()) -> do
    need [projectFile]
    parseProjectFile <$> readFile' projectFile

  -- by default, just try to compile every file in the project
  action (projectDeps (ProjectDeps ()) >>= need . Map.keys)

  phony "clean" do
    putInfo "removing artifacts"
    liftIO (removeFiles "." artifacts)
    removeFilesAfter buildDir ["//*"]

  "//*.vo" %> \out -> do
    let src = out -<.> "v"
    deps <- fileDeps (FileDeps out)
    args <- packageArgs <$> project (ProjectFile ())
    need (src : deps)
    cmd_ (Traced "rocq compile") "rocq compile -q" args src

-- dependency tracking ---------------------------------------------------------

newtype FileDeps = FileDeps FilePath
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult FileDeps = [FilePath]

newtype ProjectDeps = ProjectDeps ()
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult ProjectDeps = Map FilePath [FilePath]

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

-- project file parsing --------------------------------------------------------

newtype ProjectFile = ProjectFile ()
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult ProjectFile = Project

newtype Project = Project {packages :: [Package]}
  deriving (Show, Eq, Hashable, Binary, NFData)

-- A named package, as in '-Q theories Temporal'
data Package = Package Char FilePath String
  deriving (Show, Eq, Generic)

instance Hashable Package

instance Binary Package

instance NFData Package

parseProjectFile :: String -> Project
parseProjectFile p = Project {packages}
  where
    collect [] = ([], [], [])
    collect ("-R" : ws) = package 'R' ws
    collect ("-Q" : ws) = package 'Q' ws
    collect (path : ws)
      | "//*.v" ?== path = ([], [], [path]) <> collect ws
      | otherwise = ([], [path], []) <> collect ws

    package _ [] = error "bad -R/-Q: missing directory name"
    package c (dir : ws) = theory c dir ws

    theory _ _ [] = error "bad -R/-Q: missing theory name"
    theory c dir (name : ws) = ([Package c dir name], [], []) <> collect ws

    (packages, _directories, _files) = collect (words p)
