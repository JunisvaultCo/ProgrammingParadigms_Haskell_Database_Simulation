module Main where

import System.Environment (getArgs)
import Proof.Core
import Tasks
import Text.Printf
import qualified Data.Map as M

import qualified Refs as R
import qualified Dataset as D


task1_1 = ("Task 1", [
        expect (compute_average_steps D.eight_hours) toBeSorted R.task1_1
   ])

task1_2 = ("Task 2", [
        expect (get_passed_people_num D.eight_hours) toBe 97,
        expect (get_passed_people_percentage D.eight_hours) toBeF 0.73,
        expect (get_steps_avg D.eight_hours) toBeF 2328.19
   ])

task1_3 = ("Task 3", [
        expect (get_avg_steps_per_h D.eight_hours) toBeSorted R.task1_3
   ])

task1_4 = ("Task 4", [
        expect (get_activ_summary D.physical_activity) toBeSorted R.task1_4
   ])

task1_5 = ("Task 5", [
        expect (get_ranking D.physical_activity) toBeSorted R.task1_5
    ])

task1_6 = ("Task 6", [
        expect (get_steps_diff_table D.eight_hours) toBeSorted R.task1_6
    ])

task1_7 = ("Task 7", [
        expect (vmap (show . length) D.emails) toBeSorted R.task1_7
    ])

task1_8 = ("Task 8", [
        expect (get_sleep_total $ head $ tail $ D.sleep_min) toBeSorted R.task1_8
    ])

task2_1 = ("Task 2.1", [
        expect (tsort "TotalSteps" D.physical_activity) toBe R.task2_1
    ])

task2_2 = ("Task 2.2", [
        expect (vunion (take 100 D.physical_activity) ((head D.physical_activity):(drop 100 D.physical_activity))) toBe D.physical_activity
    ])

task2_3 = ("Task 2.3", [
        expect (hunion D.physical_activity D.eight_hours) toBe R.task2_3
    ])

task2_4 = ("Task 2.4", [
        expect (tjoin "Name" D.physical_activity D.eight_hours) toBeSorted R.task2_4
    ])

task2_5 = ("Task 2.5", [
        expect (cartesian (++) (head D.physical_activity ++ head D.eight_hours) (take 15 D.physical_activity) (take 20 D.eight_hours)) toBeSorted R.task2_5
    ])

task2_6 = ("Task 2.6", [
        expect (projection (head D.physical_activity) D.physical_activity) toBe D.physical_activity,
        expect (projection ["Name"] D.physical_activity) toBe R.task2_6
    ])

task2_7 = ("Task 2.7", [
        expect (filterTable ((>10000).read) "TotalSteps" D.physical_activity) toBe R.task2_7
    ])

test_schema2 = ["Name1", "Name2"]
names_only = (\row1 row2 -> [head row1, head row2])

task3_1 = ("Task 3.1", [
        expect (eval $ AsList "Name" $ FromTable D.physical_activity) toBe R.task3_1_1,
        expect (eval $ Sort "TotalSteps" $ FromTable D.physical_activity) toBe R.task3_1_2,
        expect (eval $ ValueMap (\x -> if x == "" then "0" else x) $ FromTable D.sleep_min) toBe R.task3_1_3,
        expect (eval $ RowMap get_sleep_total ["Name", "TotalSteps"] $ FromTable D.physical_activity) toBe R.task3_1_4,
        expect (eval $ VUnion (FromTable D.physical_activity) (FromTable D.physical_activity)) toBe R.task3_1_5,
        expect (eval $ HUnion (FromTable D.physical_activity) (FromTable D.sleep_min)) toBe R.task3_1_6,
        expect (eval $ TableJoin "Name" (FromTable D.physical_activity) (FromTable D.eight_hours)) toBe R.task3_1_7,
        expect (eval $ Cartesian names_only test_schema2 (FromTable D.physical_activity) (FromTable D.sleep_min)) toBe R.task3_1_8,
        expect (eval $ Projection ["Name", "TotalSteps"] $ FromTable D.physical_activity) toBe R.task3_1_9
    ])

task3_2 = ("Task 3.2", [
        expect (eval $ Filter (Gt "TotalMinutesAsleep1" (read "119" :: Float)) (FromTable D.sleep_min)) toBe R.task3_3_1,
        expect (eval $ Filter (Eq "Name" "Lily Luca") (FromTable D.physical_activity)) toBe R.task3_3_2,
        expect (eval $ Filter (Lt "Name" "Brian") (FromTable D.physical_activity)) toBe R.task3_3_3,
        expect (eval $ Filter (In "TotalMinutesAsleep4" [(read "515" :: Float), (read "99" :: Float)]) (FromTable D.sleep_min)) toBe R.task3_3_4,
        expect (eval $ Filter (FNot $ Eq "TotalDistance" "6.81") (FromTable D.physical_activity)) toBe R.task3_3_5,
        expect (eval $ Filter ((FieldEq "TotalMinutesAsleep3" "TotalMinutesAsleep2")::(FilterCondition String)) (FromTable D.sleep_min)) toBe R.task3_3_6
    ])

edge_op1 (n1:l1:_) (n2:l2:_)
            | l1 == l2 = Just l1
            | otherwise = Nothing

edge_op2 l1 l2
    | last l1 == last l2 = Just "identical"
    | (abs $ (read (last l1) :: Float) - (read (last l1) :: Float)) < 50 = Just "similar"
    | otherwise = Nothing

task3_4 = ("Task 3.4", [
        expect (eval $ Graph edge_op1 (FromTable D.physical_activity)) toBe R.task3_4_1--,
      --  expect (eval $ Graph edge_op2 (FromTable D.sleep_min)) toBe R.task3_4_2
    ])

task3_5 = ("Task 3.5", [
        expect (eval $ similarities_query) toBe R.task3_5
    ])

task3_6 = ("Task 3.1", [
        expect (correct_table "Name" D.emails D.physical_activity) toBe R.task3_6
    ])

taskSets2 = M.fromList [
        ("1.1", task1_1),
        ("1.2", task1_2),
        ("1.3", task1_3),
        ("1.4", task1_4),
        ("1.5", task1_5),
        ("1.6", task1_6),
        ("1.7", task1_7),
        ("1.8", task1_8),

        ("2.1", task2_1),
        ("2.2", task2_2),
        ("2.3", task2_3),
        ("2.4", task2_4),
        ("2.5", task2_5),
        ("2.6", task2_6),
        ("2.7", task2_7),

        ("3.1", task3_1),
        ("3.2", task3_2),
        ("3.4", task3_4),
        ("3.5", task3_5),
        ("3.6", task3_6)
    ]

main :: IO ()
main = do
    args <- getArgs
    let test_suites = if null args then map snd $ M.toList taskSets2
                                   else map (\x -> M.findWithDefault task1_1 x taskSets2) args

    let y = sum $ fmap (snd . fmap length) test_suites
    x <- runSuites test_suites

    putStrLn "Finished running all unit tests."

    putStrLn $ printf "%d/%d" x y
