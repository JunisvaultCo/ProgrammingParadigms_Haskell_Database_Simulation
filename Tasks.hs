
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Data.Char

import Common

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1

--  fromInteger(a):: Float is just a mouthful
to_float :: Integer -> Float
to_float a = fromInteger(a)::Float

to_float2 :: Int -> Float
to_float2 a = fromInteger(toInteger a)::Float

sum_row :: Row -> Float
sum_row r = foldr (read_nums) (to_float 0) (tail r)
  where read_nums = (\x acc -> acc + (read x)::Float)

calc_row_average_steps :: Row -> Row
calc_row_average_steps r = (head r) : [printf "%.2f" (sum_row r / (to_float 8))]

compute_average_steps :: Table -> Table
compute_average_steps m = ["Name", "Average Number of Steps"] : (map (calc_row_average_steps) (tail m))


-- Task 2

-- Number of people who have achieved their goal:

-- sum_row is defined at 1
get_passed_people_num :: Table -> Int
get_passed_people_num m = (foldr (func) 0 (tail m))
  where func x acc = if (sum_row x >= 1000) then acc + 1 else acc


-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = to_float2 (get_passed_people_num m) / (to_float2 (length m - 1))


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = foldr (calc_all_steps) 0 (tail m) / to_float2(length m - 1)
  where calc_all_steps x acc = acc + sum_row x


-- Task 3

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = (get_hour_header (tail (head m))):(get_rows (map (tail) (tail m)))
  where get_hour_header t = map ('H':) t
        get_rows t = [map (compute_avg t) (compute_sum_cols t)]
        compute_avg t = (printf "%.2f".(/ to_float2(length t)).read)
        compute_sum_cols t = (foldr (zipWith combine_rows) (head t) (tail t))
        combine_rows t1 t2 = show (read(t1) + read(t2))

-- Task 4

get_activ_summary :: Table -> Table
get_activ_summary m = ["column", "range1", "range2", "range3"]: (for_column (map (drop 3) m))
  where for_column ([]:_) = []
        for_column a = (head (head a) : (foldr (sum_column) ["0", "0", "0"] (tail a) )): for_column (map (tail) a)
        sum_column x acc =  if (read(head x) < 50) then
                            show(read(head (acc)) + 1) : tail(acc)
                          else if (read(head x) < 100) then
                             ((head acc) : [show(read(head (drop 1 acc)) + 1)]) ++ (drop 2 acc)
                          else if (read(head x) < 500) then
                             take 2 acc ++ [show(read(head (drop 2 acc)) + 1)]
                          else
                            acc


-- Task 5

compS :: String -> String -> Ordering
compS [] [] = EQ
compS n1 [] = GT 
compS [] n2 = LT 
compS n1 n2 = if (head (n1) > head (n2)) then
                GT 
              else if (head (n1) < head(n2)) then
                LT
              else
                compS (tail n1) (tail n2)

ordering_total_steps :: Row -> Row -> Ordering 
ordering_total_steps t1 t2 = comp (read (head (tail t1))::Float) (head t1) (read (head (tail t2))::Float) (head t2)
  where comp steps1 name1 steps2 name2 = if (steps1 == steps2) then
                                           compS (name1) (name2)
                                         else if (steps1 > steps2) then
                                           GT
                                         else LT

get_ranking :: Table -> Table
get_ranking m = ["Name", "Total Steps"]:(map (take 2) (sortBy (ordering_total_steps) (tail m)))


-- Task 6


ordering_diff :: Row -> Row -> Ordering 
ordering_diff t1 t2 = if ((comp t1 t2) > 0) then
                        GT
                      else if ((comp t1 t2) < 0) then
                        LT
                      else
                        compS (head t1) (head t2)
  where comp t1 t2 = (read (head (drop 3 t1))::Float) - read (head (drop 3 t2))::Float

get_steps_diff_table_aux :: Table -> Table
get_steps_diff_table_aux t = map (row) (tail t)
  where row x = (head x): ([printf "%.2f" (first4 x), printf "%.2f" (last4 x), dif (first4 x) (last4 x)])
        first4 r = (sum_row (take 5 r)) / (to_float 4) -- sum_row removes the first column
        last4 r = (sum_row (drop 4 r)) / (to_float 4)
        dif f4 l4 = printf "%.2f" (abs(l4 - f4))

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = header:sortBy (ordering_diff) (r m)
  where header = ["Name", "Average first 4h", "Average last 4h", "Difference"]
        r t = get_steps_diff_table_aux t

-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : map (f) (tail m)

get_sleep_total :: Row -> Row
get_sleep_total r = head(r): [printf ("%.2f") (sum_row r)]

{-
    TASK SET 2
-}

-- Task 1

-- concept: I'm creating a new table in which each row is actually a pair of two rows, first
-- from the column we are sorting by and the second is the full row. This allows to easily
-- order by first column when the result for the desired column is actually the same.

ordering_element :: Value -> Value -> Ordering
ordering_element x y | (length x == 0) && (length y == 0) = EQ
                     | length x == 0 = LT
                     | length y == 0 = GT
                     | foldr (\z acc -> if not (isDigit z) then True else acc) False x = compS x y
                     | ((read x)::Float) > (read (y)::Float) = GT
                     | ((read x)::Float) == ((read y)::Float) = EQ 
                     | otherwise = LT

ordering_column :: (Row, Row) -> (Row, Row) -> Ordering
ordering_column (w, x) (y, z) = if ordering_element (head w) (head y) == EQ then
                                    ordering_element (head x) (head z)
                                else
                                    ordering_element (head w) (head y)

cut_until_column :: ColumnName -> Table -> Table
cut_until_column column table = foldr (aux column) table (head table)
    where aux column x acc = if ((head (head acc)) == column) then acc else map (drop 1) acc

tsort :: ColumnName -> Table -> Table
tsort column table = (head table): (revert (sortBy (ordering_column) (prepare table)))
    where revert table = map (\(x,y)->y) table
          prepare table = (zipWith (,) (tail (cut_until_column column table)) (tail table))

-- Task 2

verify_same_columns :: Table -> Table -> Bool
verify_same_columns t1 t2 = foldr (&&) True (zipWith (==) (head t1) (head t2))

vunion :: Table -> Table -> Table
vunion t1 t2 = if (verify_same_columns t1 t2 && (length (head t1) == (length (head t2)))) then
                   t1 ++ (tail t2)
               else
                   t1

-- Task 3

perform_padding :: Int -> Table -> Table
perform_padding i t1 = t1 ++ (take i (repeat (take (length (head t1)) (repeat ""))))

hunion :: Table -> Table -> Table
hunion t1 t2 = if (length t1 > length t2) then
                  zipWith (++) t1 (perform_padding (length t1 - length t2) t2)
               else if (length t1 < length t2) then
                  zipWith (++) (perform_padding (length t2 - length t1) t1) t2
               else
                  zipWith (++) t1 t2

-- Task 4

-- requires a header and will take the next row after header so
-- ideally give a table with 2 rows. Returns the value from that
-- column for the first row below the header, or "" if there is
-- none.
get_key :: ColumnName -> Table -> Value
get_key key_column t = if length (head (cut_until_column key_column t)) == 0 then
                           ""
                       else
                           head (head (tail (cut_until_column key_column t)))

-- merges headers without duplicates making sure that the columns that
-- appear at least in t1 are first. Wants an array of rows but really it
-- only uses the first row (usually shorter to just give it the entire table)
merge_headers :: Table -> Table -> Row
merge_headers t1 t2 = (head t1) ++ (foldr (\x acc ->
                                   if not (foldr (\y acc2 -> acc2 || (y == x)) False (head t1)) then
                                       [x] ++ acc
                                   else
                                       acc) [] (head t2))

-- merges 2 rows GIVEN AS HEADER: ACTUAL_ROW
merge_row :: Table -> Table -> Row
merge_row r1 r2 = map (\x -> if (get_key x r2) == "" then get_key x r1 else get_key x r2) (merge_headers r1 r2)

-- just walks through t1, removing any rows from t2 when they are merged,
-- until there are no more rows in at least one of the tables.
tjoin_no_head :: ColumnName -> Table -> Table -> Table
tjoin_no_head key_column t1 [_] = []
tjoin_no_head key_column [_] t2 = []
tjoin_no_head key_column t1 t2 = if (length (search (get_key key_column t1) key_column t2) /= 0) then
                                 -- if there is an entry with same key in t2, merge those
                                     merge_row (take 2 t1) (search (get_key key_column t1) key_column t2) : next_level
                                 else
                                 -- otherwise don't save this entry from t1.
                                     next_level
    where next_level = tjoin_no_head key_column (cut_first_row t1) (remove key_column (get_key key_column t1) t2)
          remove key_column column t = foldr (remove_aux column (head t)) [head t] (tail t)
          remove_aux column h x acc = if get_key key_column (h : [x]) == column then acc else acc ++ [x]
          cut_first_row t = (head t) : (drop 2 t)
          -- we always need to keep the header for get_key (cut_until_column) to work.
          search v column t = foldr (\x acc -> if get_key column (head t:[x]) == v then (head t: [x]) else acc) [] (tail t)

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = (merge_headers t1 t2): (tjoin_no_head key_column t1 t2)

-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names: foldr (cartesian_row new_row_function (tail t2) ) [] (tail t1)
    where cartesian_row f t2 r acc = foldr (\x acc2 -> [f r x] ++ acc2) acc t2

-- Task 6

extract_col :: ColumnName -> Table -> Table
extract_col column t = map (take 1) (cut_until_column column t)

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = foldr (projection_aux t) (create_acc t) columns_to_extract
    where projection_aux t x acc = zipWith (++) (extract_col x t) acc
          create_acc t = take (length t) (repeat [])

-- Task 7

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = head t : (foldr (aux) [] (tail t))
    where aux x acc = if condition(head(head (tail (cut_until_column key_column ((head t):[x]))))) then
                          [x] ++ acc
                      else
                          acc

{-
    TASK SET 3
-}


-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5

instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

getTable :: QResult -> Table
getTable (Table table) = table
getTable (List list) = []

class Eval a where
    eval :: a -> QResult

instance Eval Query where
    eval (FromTable table) = Table table
    -- need tail so it doesn't add header too
    eval (AsList colname query) = List (getColAsList (tail (extract_col colname (getTable (eval query)))))
        where getColAsList table = map (head) table
    eval (Sort colname query) = Table (tsort colname (getTable (eval query)))
    eval (ValueMap op query) = Table (vmap (op) (getTable (eval query)))
    eval (RowMap op colnames query) = Table (rmap (op) colnames (getTable (eval query)))
    eval (VUnion query1 query2) = Table (vunion (getTable (eval query1)) (getTable (eval query2)))
    eval (HUnion query1 query2) = Table (hunion (getTable (eval query1)) (getTable (eval query2)))
    eval (TableJoin colname query query2) = Table (tjoin colname (getTable (eval query)) (getTable (eval query2)))
    eval (Cartesian op colnames query1 query2) = Table (cartesian (op) colnames (getTable (eval query1)) (getTable (eval query2)))
    eval (Projection colnames query) = Table (projection colnames (getTable (eval query)))
    eval (Filter fcond query) = Table ((head (getTable (eval query))):(filter (feval (head (getTable (eval query))) fcond) (tail (getTable (eval query)))))
    eval (Graph op query) = Table (["From", "To", "Value"]:(graph_aux (op) (getTable (eval query))))
      where graph_aux op [] = []
            graph_aux op (x:rest) = (foldr (\x2 acc ->
                      if (isnt_nothing (op x x2)) then
                        if (compS (head x) (head x2)) == GT then
                          [[head x2, head x, extract (op x x2)]] ++ acc
                        else
                          [[head x, head x2, extract (op x x2)]] ++ acc
                      else
                        acc) [] rest) ++ (graph_aux (op) (rest))
            isnt_nothing (Nothing) = False
            isnt_nothing (Just a) = True
            extract (Just a) = a
            extract (Nothing) = "Nothing"

            
-- 3.2 & 3.3

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

extract_col_string :: ColumnName -> Table -> String
extract_col_string column t = head (head (tail (extract_col column t)))

extract_col_float :: ColumnName -> Table -> Float
extract_col_float column t = (read (extract_col_string column t))::Float

aux_feval_float :: [String] -> (FilterCondition Float) -> Row -> Bool
aux_feval_float rows (Eq col num) row = (extract_col_float col (rows:[row])) == num
aux_feval_float rows (Lt col num) row = (extract_col_float col (rows:[row])) < num
aux_feval_float rows (Gt col num) row = (extract_col_float col (rows:[row])) > num
aux_feval_float rows (In col nums) row = foldr (\x acc -> acc || (x == (extract_col_float col (rows:[row])))) False nums
aux_feval_float rows (FNot fcond) row = not (aux_feval_float rows fcond row)
aux_feval_float rows (FieldEq col1 col2) row = (extract_col_float col1 (rows:[row])) == (extract_col_float col2 (rows:[row]))

aux_feval_string :: [String] -> (FilterCondition String) -> Row -> Bool
aux_feval_string rows (Eq col num) row = (extract_col_string col (rows:[row])) == num
aux_feval_string rows (Lt col num) row = (extract_col_string col (rows:[row])) < num
aux_feval_string rows (Gt col num) row = (extract_col_string col (rows:[row])) > num
aux_feval_string rows (In col nums) row = foldr (\x acc -> acc || (x == (extract_col_string col (rows:[row])))) False nums
aux_feval_string rows (FNot fcond) row = not (aux_feval_string rows fcond row)
aux_feval_string rows (FieldEq col1 col2) row = (extract_col_string col1 (rows:[row])) == (extract_col_string col2 (rows:[row]))


instance FEval Float where
    feval rows fcond = (\row -> aux_feval_float rows fcond row)

instance FEval String where
    feval rows fcond = (\row -> aux_feval_string rows fcond row)

-- 3.4

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

-- 3.5

similarities_op :: Row -> Row -> Maybe Value
similarities_op r1 r2 = Just (show (foldr (\x acc -> if (x) then acc + 1 else acc) 0 (
    zipWith (\x y -> ((read x)::Float) == ((read y)::Float)) (tail r1) (tail r2))))

similarities_query :: Query
similarities_query = Sort "Value" (Filter (Gt "Value" (4::Float)) (
        Graph (similarities_op) (FromTable eight_hours)
      ))

-- 3.6 (Typos)

get_list :: QResult -> [String]
get_list (List l) = l

transform_line_to_table :: [String] -> Table
transform_line_to_table l = ["Name"]:(map (\x -> x:[]) l)

-- get the ones from the typoed that are remaining and the ones from the non-typoed that are remaining
-- this will be a list of two lists, the first being with the typoed names
get_problematic :: [String] -> [String] -> Table
get_problematic l ref = [get_list (eval (AsList "Name" (Filter (FNot (In "Name" ref)) (FromTable (transform_line_to_table l))))),
                          get_list (eval (AsList "Name" (Filter (FNot (In "Name" l)) (FromTable (transform_line_to_table ref)))))]

-- sweet regrets are made of this
-- who am I to disagree
-- Concept: each delete or adding is "of cost 1". Minimum among those.
calc_edit_dist_aux :: String -> String -> String -> [Integer] -> [Integer] -> Integer
calc_edit_dist_aux a [] a_full bef current = last bef
calc_edit_dist_aux [] b a_full [] current = calc_edit_dist_aux a_full b a_full current []
calc_edit_dist_aux [] b a_full bef current = calc_edit_dist_aux a_full (tail b) a_full current []
calc_edit_dist_aux a b a_full [] [] = calc_edit_dist_aux a b a_full [] [0]
calc_edit_dist_aux a b a_full [] current = calc_edit_dist_aux (tail a) b a_full [] (current ++ [last current + 1])
calc_edit_dist_aux a b a_full bef [] = calc_edit_dist_aux a b a_full bef ([head bef + 1])
calc_edit_dist_aux a b a_full bef current = if (head a) == (head b) then
                                              calc_edit_dist_aux (tail a) b a_full (tail bef) (current ++ [head bef])
                                            else
                                              calc_edit_dist_aux (tail a) b a_full (tail bef) (current ++ [min (head (drop 1 bef)) (last current) + 1])

calc_edit_dist :: String -> String -> Integer
calc_edit_dist a b = calc_edit_dist_aux a b a [] []

get_matches_aux :: [String] -> String -> String
get_matches_aux ref x = foldr (\y acc -> if (calc_edit_dist x y) > (calc_edit_dist x acc) then acc else y) (head ref) (tail ref)

get_matches :: [[String]] -> [[String]]
get_matches (t:[ref]) = zipWith (:) (map (get_matches_aux ref) t) (map (:[]) t)

change_to :: String -> [[String]] -> String
change_to x refs = foldr (\y acc -> if (last y) == x then head y else acc) x refs

rest_from_cut :: ColumnName -> Table -> Table
rest_from_cut column table = foldr (aux column) table (head table)
    where aux column x acc = if (last (head acc)) == column then acc else map (take (length acc - 1)) acc

switch_table :: ColumnName -> Table -> [[String]] -> Table 
switch_table col t matches = head t : (map (change_elem) (tail t))
    where change_elem x = (take (length (change_last x) - 1) (change_last x)) ++ [new_elem x] ++ (change_first x)
          change_last el = head (tail (rest_from_cut col ((head t):[el])))
          change_first el = tail (head (tail (cut_until_column col ((head t):[el]))))
          new_elem el = change_to (head (head (tail (cut_until_column col ((head t):[el]))))) matches

correct_table :: String -> Table -> Table -> Table
correct_table col t ref = switch_table col t
  (get_matches (get_problematic (get_list (eval (AsList col (FromTable t)))) (get_list (eval (AsList col (FromTable ref))))))
