module Main where

import Test.Tasty
import Test.Tasty.Golden 
import Test.Tasty.HUnit

import System.FilePath
import System.Process
import System.Directory

import Parser
import Compile

main :: IO()
main = do
	testSetup
	defaultMain =<< diffTest

diffTest :: IO TestTree
diffTest = do
	-- gets all result files (compiled and run files)
	regressions <- findByExtension [".ref"] "./test/regression/features"
	-- list comprehension running golden diff for each result file generated. 
	return $ testGroup "diff golden tests" [ goldenVsFile ("diffing " ++ (takeBaseName golden)) golden output dummy 
		| golden <- regressions, let output = replaceExtension golden ".res"]

dummy :: IO ()
dummy = return ()

testSetup :: IO ()
testSetup = do
	-- get all racket files from top level
	regressions <- findByExtension [".rkt"] "./test/regression/features"
	--make filepath absolute
	regressions <- mapM makeAbsolute regressions
	-- compile all racket files after changing the extensions to .run for make
	compileFiles $ replaceExt regressions "run"
	
	
--calls make on each filepath given. 
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
	_ <- createProcess (proc "racket" [fp, ">", ref]) { cwd = Just "." } -- dont need cwd path since paths are absolute anyway
	print ("compiled canonical output for " ++ fp)


