module CountVectorizer (CountVectorizer, printer, lookUp, flatten, fitVectorizer, encode) where

import qualified Data.Map (Map, empty, fromList, insert, lookup, size, toList, traverseWithKey, (!))
import qualified Data.Vector (Vector, empty, fromList, replicate, update, (!))

type Dictionary = Data.Map.Map Int String
type CountMap = Data.Map.Map String Int
type CountVector = Data.Vector.Vector Int

data CountVectorizer = CountVectorizer Dictionary CountVector Int
    deriving (Show, Eq)

printer :: CountVectorizer -> Dictionary
printer (CountVectorizer d _ _) = d

lookUp :: Int -> CountVectorizer -> String 
lookUp i (CountVectorizer dic _ _) = case Data.Map.lookup i dic of
    Just s -> s
    Nothing -> error $ show i

buildCountMap :: [String] -> CountMap
buildCountMap =
    foldl
        ( \wordMap word -> case Data.Map.lookup word wordMap of
            Just count -> Data.Map.insert word (count + 1) wordMap
            Nothing -> Data.Map.insert word 1 wordMap
        )
        Data.Map.empty

fitVectorizer :: [String] -> CountVectorizer
fitVectorizer text =
    let countMap = buildCountMap text
        cmap = Data.Map.toList countMap
        num_words = Data.Map.size countMap
        (dict, counts) =
            foldl
                ( \(dict, counts) (word, count) ->
                    ( Data.Map.insert (num_words - (length counts) - 1) word dict
                    , count : counts
                    )
                )
                (Data.Map.empty, [])
                cmap
     in CountVectorizer dict (Data.Vector.fromList counts) num_words

indexToCount :: Int -> Dictionary -> CountMap -> Int
indexToCount index dict countMap =
    case Data.Map.lookup index dict of
        Just word ->
            case Data.Map.lookup word countMap of
                Just count -> count
                Nothing -> 0
        Nothing -> 0

encode :: String -> CountVectorizer -> CountVector
encode text vectorizer = Data.Vector.fromList (encodeHelper (buildCountMap (words text)) vectorizer [] 0)

encodeHelper :: CountMap -> CountVectorizer -> [Int] -> Int -> [Int]
encodeHelper cmap (CountVectorizer dict counts num_words) encoded index =
    if (index == num_words)
        then encoded
        else
            ( let count = indexToCount index dict cmap
               in count : encodeHelper cmap (CountVectorizer dict counts num_words) encoded (index + 1)
            )

-- takes a list of sentences and returns a list of words
flatten :: [String] -> [String]
flatten = foldr ((++) . words) []