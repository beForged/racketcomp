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
	regressions <- findByExtension [".res"] "./test/regression/features"
	return $ testGroup "diff golden tests" [ goldenVsFile ("diffing " ++ (takeBaseName output)) golden output dummy | output <- regressions, let golden = replaceExtension output ".ref"]

dummy :: IO ()
dummy = return ()

testSetup = do
	-- get all racket files from top level
	regressions <- findByExtension [".rkt"] "./test/regression/features"
	--make filepath absolute
	regressions <- mapM makeAbsolute regressions
	-- compile all racket files after changing the extensions to .run for make
	compileFiles $ replaceExt regressions
	
	
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
replaceExt :: [FilePath] -> [FilePath]
replaceExt (x:xs) = (replaceExtension x "run") : replaceExt xs
replaceExt [] = []


