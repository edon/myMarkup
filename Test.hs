import Parser
import XMLBackend 
import System.Directory
import Control.Applicative
import Data.List

testsDir = "tests/"

runTest :: String -> String -> Bool
runTest txt xml = 
    case parseMarkup txt of
      Left _ -> False
      Right r -> xml == markupToXML r

testFile :: String -> IO Bool
testFile s = runTest <$> readFile (s ++ ".txt") <*> readFile (s ++ ".xml")

testsNames :: IO [String]
testsNames =
    sort <$> 
    map (takeWhile (/= '.')) <$>
    filter (\s -> isSuffixOf ".xml" s) 
               <$> ((++ "/" ++ testsDir) 
               <$> getCurrentDirectory 
               >>= getDirectoryContents)

printSingleTest :: String -> IO String
printSingleTest s = do
  b <- testFile $ testsDir ++ s
  if b then (return $ s ++ "  ----  PASS\n")
       else (return $ s ++ "  ---- FAIL\n")

runAllTests :: IO String
runAllTests = do 
  ts <- testsNames
  ps <- sequence $ printSingleTest <$> ts 
  return $ concat ps
