{-# Language OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.Golden 
import Test.Tasty.HUnit

import System.FilePath
import System.Process
import System.Directory

import qualified Turtle

import Parser
import Compile

main :: IO()
main = do
	testSetup
	defaultMain =<< diffTest
	--remove .run and .res files after running (maybe remove .ref? would allow for changing tests)
	--all are removed since they change with each compiler change
	--TODO only remove if test passes
	cleanDir "./test/regression/features" [".run", ".res"]

--removes reference files
fullClean :: IO()
fullClean = cleanDir "./test/regression/features" [".run", ".res", ".ref"]

diffTest :: IO TestTree
diffTest = do
	-- gets all result files (compiled and run files)
	regressions <- findByExtension [".rkt"] "./test/regression/features"
	--TODO add other tests
	features <- findByExtension [".rkt"] "./test/regression/programs"
	-- list comprehension running golden diff for each reference file found
	return $ testGroup "diff golden tests" [ goldenVsFile ("diffing " ++ (takeBaseName golden)) golden output dummy 
		| golden <- replaceExt regressions ".ref", let output = replaceExtension golden ".res"]

-- does nothing since makefile calls the compilation
dummy :: IO ()
dummy = return ()

testSetup :: IO ()
testSetup = do
	-- get all racket files from top level
	regressions <- findByExtension [".rkt"] "./test/regression/features"
	-- make filepath absolute
	regressions <- mapM makeAbsolute regressions
	-- generate canonical (reference) output files
	compileReferences regressions
	-- compile all racket files after changing the extensions to .run for make
	compileFiles $ replaceExt regressions ".run"
	-- generate result files for diffing
	-- TODO probably need turtle for this
	-- runExes (replaceExt regressions ".run") (replaceExt regressions ".res")
	_ <- Turtle.shell "./test/regression/features/runner.sh" Turtle.empty
	print "test setup complete"

-- should never be differing list size since both are transformed from same list
runExes :: [FilePath] -> [FilePath] -> IO ()
runExes [] [] = print "files run"
runExes [x] [y] = runExe x y
runExes (x:xs) (y:ys) = do
	runExe x y
	runExes xs ys


-- no existence check since it may have to be updated
runExe :: FilePath -> FilePath -> IO ()
runExe run res = do
	let comm = run ++ " > " ++ res
	_ <- createProcess (shell comm)
	print ("generate result file from " ++ run)

	
--calls make on each filepath given. Generates executable compiled .run files
compileFiles :: [FilePath] -> IO ()
compileFiles [] = print "files compiled"
compileFiles [x] = do
	_ <- createProcess (proc "make" [x]) { cwd = Just "./src" }
	compileFiles []
compileFiles (x:xs) = do
	_ <- createProcess (proc "make" [x]) { cwd = Just "./src" }
	compileFiles xs

--replace .rkt extension with .run for the makefile
replaceExt :: [FilePath] -> String -> [FilePath]
replaceExt (x:xs) str = (replaceExtension x str) : replaceExt xs str
replaceExt [] _ = []




-- generate canonical (reference) files using racket
compileReferences :: [FilePath] -> IO ()
compileReferences [] = print "reference files compiled"
compileReferences [x] = compileReference x
compileReferences (x:xs) = do
	_ <- compileReference x
	compileReferences xs


compileReference :: FilePath -> IO ()
compileReference fp = do
	-- check if file exists already, since no point in recompiling
	let ref = replaceExtension fp ".ref"
	doesRefExist <- doesFileExist ref
	if not doesRefExist 
		then helper fp ref
		else return () --do nothing
		
helper :: FilePath -> FilePath -> IO ()
helper fp ref = do 
	let path = Turtle.decodeString fp
	let res = Turtle.decodeString ref
	let cmd = Turtle.format ("racket "Turtle.% Turtle.fp Turtle.%" > "Turtle.% Turtle.fp) path res
	_ <- Turtle.shell cmd Turtle.empty
--	_ <- createProcess (proc "racket" [fp, ">", ref]) { cwd = Just "." } -- dont need cwd path since paths are absolute anyway
	print ("compiled canonical output for " ++ fp)



-- remove all files with extensions in filepath directory
cleanDir :: FilePath -> [FilePath] -> IO ()
cleanDir fp str = do
	res <- findByExtension str fp
	mapM removeFile res
	--print output here?
	return ()

