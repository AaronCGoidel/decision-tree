import CountVectorizer (CountVectorizer, encode, fitVectorizer, flatten, lookUp, printer)

import qualified Data.List (maximumBy, partition)
import qualified Data.Map (Map, empty, foldr, fromList, insert, lookup, toList)
import qualified Data.Vector (Vector, (!))

shuffle :: [a] -> [a] -> [a]
shuffle [] ys = ys
shuffle xs [] = xs
shuffle (x : xs) (y : ys) = x : y : shuffle xs ys

splitAndShuffle :: [a] -> Int -> [a]
splitAndShuffle lst at = shuffle xs ys where (xs, ys) = splitAt ((length lst) `div` at) lst

data DTree = Internal Split DTree DTree | Leaf String
    deriving (Eq)
instance Show DTree where
    show (Leaf c) = c
    show (Internal split left right) = unlines (prettyprint_helper (Internal split left right))

prettyprint_helper :: DTree -> [String]
prettyprint_helper (Internal split left right) =
    (show split) : (prettyprint_subtree left right)
  where
    prettyprint_subtree left right =
        ((pad "+- " "|  ") (prettyprint_helper right))
            ++ ((pad "`- " "   ") (prettyprint_helper left))
    pad first rest = zipWith (++) (first : repeat rest)
prettyprint_helper (Leaf c) =
    [c]

type X = Data.Vector.Vector Int
type TrainMapping = Data.Map.Map X String

data TrainData = TrainData [X] TrainMapping

data Split = Split Int Float Group Group
    deriving (Eq)
instance Show Split where
    show (Split attr ig _ _) = "Split on \"" ++ show attr ++ "\" IG: " ++ show ig

data Group = Group [X] (Data.Map.Map String Int)
    deriving (Show, Eq)

-- return the key of the max value in a map
getMaxFromMap :: Data.Map.Map String Int -> String
getMaxFromMap m = fst $ Data.List.maximumBy (\(_, a) (_, b) -> compare a b) $ Data.Map.toList m

groupSize :: Group -> Int
groupSize (Group xs m) = length xs

prepData :: [a] -> (a -> Data.Vector.Vector Int) -> String -> TrainData -> TrainData
prepData [] encoder tag train = train
prepData (dat : dats) encoder tag (TrainData xs mappings) =
    let encoded = encoder dat
     in prepData dats encoder tag (TrainData (encoded : xs) (Data.Map.insert encoded tag mappings))

insertOrUpdate :: Data.Map.Map String Int -> String -> Data.Map.Map String Int
insertOrUpdate map tag = Data.Map.insert tag (maybe 1 (+ 1) (Data.Map.lookup tag map)) map

countClassMembership :: TrainData -> Group
countClassMembership (TrainData xs mappings) =
    Group
        xs
        ( foldl
            ( \m x -> case Data.Map.lookup x mappings of
                Nothing -> m
                Just y -> insertOrUpdate m y
            )
            Data.Map.empty
            xs
        )

splitOnFeature :: Group -> TrainMapping -> Int -> Float -> (Group, Group) -> (Group, Group)
splitOnFeature (Group [] _) _ _ _ ret = ret
splitOnFeature (Group (x : xs) counts) mapping feat threshold (Group left lmap, Group right rmap) = case (fromIntegral (x Data.Vector.! feat) < threshold, Data.Map.lookup x mapping) of
    (True, Just tag) -> splitOnFeature (Group xs counts) mapping feat threshold (Group (x : left) (insertOrUpdate lmap tag), Group right rmap)
    (False, Just tag) -> splitOnFeature (Group xs counts) mapping feat threshold (Group left lmap, Group (x : right) (insertOrUpdate rmap tag))
    _ -> splitOnFeature (Group xs counts) mapping feat threshold (Group left lmap, Group right rmap)

entropy :: Group -> Float
entropy (Group xs class_counts) =
    sum $
        map
            ( \(tag, count) ->
                let p = fromIntegral count / fromIntegral (length xs)
                 in - p * logBase 2 p
            )
            (Data.Map.toList class_counts)

bestSplit :: Group -> TrainMapping -> Split -> Split
bestSplit group map = bestSplitThresholdHelper group map 0 0

bestSplitThresholdHelper :: Group -> TrainMapping -> Int -> Int -> Split -> Split
bestSplitThresholdHelper group map feat row_num best_split = if row_num >= (groupSize group) then
    best_split else (
        let (Group xs counts) = group
            threshold = fromIntegral $ (xs !! row_num) Data.Vector.! feat
            (Split best_feat thresh left right) = bestSplitHelper (Group xs counts) map feat (fromIntegral threshold) best_split
            left_len = groupSize left    
            right_len = groupSize right
            tot_len = left_len + right_len
            in if left_len == 0 || right_len == 0
                    then best_split
                    else
                        let left_entropy = entropy left
                            right_entropy = entropy right
                            split_ig = (entropy (Group xs counts)) - (fromIntegral (left_len) / fromIntegral (tot_len)) * left_entropy - (fromIntegral (right_len) / fromIntegral (tot_len)) * right_entropy
                            (Split _ curr_ig _ _) = best_split
                         in if split_ig > curr_ig
                                then bestSplitThresholdHelper (Group xs counts) map feat (row_num + 1) (Split feat split_ig left right)
                                else bestSplitThresholdHelper (Group xs counts) map feat (row_num + 1) best_split
                
    )
bestSplitHelper :: Group -> TrainMapping -> Int -> Int -> Split -> Split
bestSplitHelper (Group xs counts) mapping feat row_num curr_best =
    if feat == (length (head xs))
        then curr_best
        else
            let (left, right) = splitOnFeature (Group xs counts) mapping feat (fromIntegral $ (xs !! row_num) Data.Vector.! feat) (Group [] (Data.Map.empty), Group [] (Data.Map.empty))
                left_len = length $ (\(Group x _) -> x) left
                right_len = length $ (\(Group x _) -> x) right
                tot_len = length xs
             in if left_len == 0 || right_len == 0
                    then curr_best
                    else
                        let left_entropy = entropy left
                            right_entropy = entropy right
                            split_ig = (entropy (Group xs counts)) - (fromIntegral (left_len) / fromIntegral (tot_len)) * left_entropy - (fromIntegral (right_len) / fromIntegral (tot_len)) * right_entropy
                            (Split _ curr_ig _ _) = curr_best
                         in if split_ig > curr_ig
                                then bestSplitHelper (Group xs counts) mapping (feat + 1) row_num (Split feat split_ig left right)
                                else bestSplitHelper (Group xs counts) mapping (feat + 1) row_num curr_best

generateRoot :: TrainMapping -> Group -> DTree
generateRoot mapping group  = Internal (bestSplit group mapping emptySplit) (Leaf "") (Leaf "")

toTerminal :: Group -> TrainMapping -> DTree
toTerminal (Group xs counts) mapping = Leaf (getMaxFromMap counts)

splitDown :: DTree -> Int -> Int -> Int -> TrainMapping -> DTree
splitDown (Leaf tag) _ _ _ _ = Leaf tag
splitDown (Internal curr_split _ _) max_depth depth min_group_size mapping =
    let (Split attr_no ig left_split right_split) = curr_split
     in ( if depth >= max_depth
            then Internal curr_split (toTerminal left_split mapping) (toTerminal right_split mapping)
            else
                ( case (groupSize left_split < min_group_size, groupSize right_split < min_group_size) of
                    (True, True) -> Internal curr_split (toTerminal left_split mapping) (toTerminal right_split mapping)
                    (True, False) -> Internal curr_split (toTerminal left_split mapping) (splitDown (generateRoot mapping right_split ) max_depth (depth + 1) min_group_size mapping)
                    (False, True) -> Internal curr_split (splitDown (generateRoot mapping left_split ) max_depth (depth + 1) min_group_size mapping ) (toTerminal right_split mapping)
                    (False, False) ->
                        let left_split' = splitDown (generateRoot mapping left_split ) max_depth (depth + 1) min_group_size mapping 
                            right_split' = splitDown (generateRoot mapping right_split ) max_depth (depth + 1) min_group_size mapping 
                         in Internal curr_split left_split' right_split'
                )
        )

emptySplit :: Split
emptySplit = Split 0 (-2) (Group [] Data.Map.empty) (Group [] Data.Map.empty)

buildTree :: TrainData -> DTree
buildTree (TrainData xs mappings) =
    let root = generateRoot mappings (countClassMembership (TrainData xs mappings))
     in splitDown root 5 0 5 mappings

main :: IO ()
main = do
    -- raw_fake <- readFile "data/fake_title_sm.txt"
    -- raw_real <- readFile "data/real_title_sm.txt"

    -- let fake_lines = lines raw_fake
    -- let real_lines = lines raw_real

    let fake_lines = ["foo", "beep foo", "foo beep", "beep", "hello world", "this sentence is false", "this one is not", "something or other is in this sentence which is longer than the rest"]
    let real_lines = ["foo bar beep", "bar", "else beep bar", "nuts candy", "nuts without candy or something"]
    let all_lines = fake_lines ++ real_lines

    let vectorizer = fitVectorizer (flatten all_lines)
    -- print $ printer vectorizer
    let fake_data = prepData fake_lines (\str -> encode str vectorizer) "fake" (TrainData [] Data.Map.empty)
    let (TrainData all_data tag_map) = prepData real_lines (\str -> encode str vectorizer) "real" fake_data
    -- -- print $ lmap
    -- let g = (countClassMembership (TrainData all_data tag_map))
    -- print $ show $ groupSize g
    let (Split feat ig left right) = bestSplit (countClassMembership (TrainData all_data tag_map)) tag_map emptySplit
    print $ show ig
    print $ lookUp feat vectorizer
    -- buildTree
    -- print $ generateRoot tag_map (countClassMembership (TrainData all_data tag_map)) vectorizer
    -- print $ buildTree (TrainData all_data tag_map)