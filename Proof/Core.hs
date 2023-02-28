module Proof.Core (
    expect,
    runSuite,
    runSuites,
    module Proof.Matchers
) where

import Data.List (partition)
import Proof.Matchers
import Proof.Message.Format
import Control.DeepSeq
import Data.Time
import Control.Exception

type Passed = [String]
type Failed = [String]
type Suite = [String]

expect :: (Show actual, Show expected) =>  actual -> Matcher actual expected -> expected -> String
expect actual (failMessage, evaluator) expected
    | evaluator actual expected = "Passed"
    | otherwise                 = "Failed: " ++
        (format
            failMessage
            [("@expected@", show expected), ("@actual@", show actual)]
        )

printResults :: String -> (Passed, Failed) -> IO Int
printResults suite_name (passed, failed) =
        do
        let numPassed = length passed
        let numFailed = length failed

        (putStrLn . unlines) [
            if (length failed == 0) then "All tests passed.\n" else "There were test failures:\n\n" ++ unlines failed,
            "Passed:\t\t" ++ (show numPassed),
            "Failed:\t\t" ++ (show numFailed),
            "Tests Run:\t" ++ (show (numPassed + numFailed))
            ]

        return numPassed

runSuite :: (String, Suite) -> IO Int
runSuite (suite_name, suite) = do
    putStrLn $ "###### Running testsuite for " ++ suite_name
    ((printResults suite_name) . partition (=="Passed") ) suite

runSuites :: [(String, Suite)] -> IO Int
runSuites [] = do putStrLn ""; return 0
runSuites (suite:suites) =
    do
        start <- getCurrentTime
        x <- runSuite suite
        --print (diffUTCTime end start)
        y <- runSuites suites

        return (x + y)
