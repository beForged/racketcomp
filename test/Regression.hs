module Main where

import Test.Tasty
import Test.Tasty.Golden 

import System.FilePath

main :: IO()
main = defaultMain =<< goldenTests



goldenTests :: IO TestTree
goldenTests = do
	regressions <- findByExtension 	[".rkt"] "."
	return $ testGroup "rkt"
