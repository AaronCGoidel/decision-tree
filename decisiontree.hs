import qualified Data.Map (Map, empty, fromList, insert, lookup, size, toList, traverseWithKey)
import qualified Data.Vector (Vector, empty, fromList, (!))

data DTree = Internal String Float [DTree] | Leaf Float
    deriving (Show, Eq)

data Pair = Pair Int Int
    deriving (Show, Eq)

type Dictionary = Data.Map.Map String Int
type CountMap = Data.Map.Map String Int
type CountVector = Data.Vector.Vector Int

data CountVectorizer = CountVectorizer Dictionary CountVector
    deriving (Show, Eq)

buildCountMap :: [String] -> CountMap
buildCountMap =
    foldl
        ( \wordMap word -> case Data.Map.lookup word wordMap of
            Just count -> Data.Map.insert word (count + 1) wordMap
            Nothing -> Data.Map.insert word 1 wordMap
        )
        Data.Map.empty

fitVectorizer :: CountMap -> CountVectorizer
fitVectorizer countmap =
    let cmap = Data.Map.toList countmap
        num_words = Data.Map.size countmap
        (dict, counts) =
            foldl
                ( \(dict, counts) (word, count) ->
                    ( Data.Map.insert word (num_words - (length counts) - 1) dict
                    , count : counts
                    )
                )
                (Data.Map.empty, [])
                cmap
     in CountVectorizer dict (Data.Vector.fromList counts)

-- takes a list of sentences and returns a list of words
flatten :: [String] -> [String]
flatten = foldr ((++) . words) []

main :: IO ()
main = do
    raw_fake <- readFile "data/fake_title.txt"
    raw_real <- readFile "data/real_title.txt"
    let fake = lines raw_fake
    let real = lines raw_real

    let fake_words = flatten fake
    let real_words = flatten real

    let fake_count_map = buildCountMap fake_words
    let real_count_map = buildCountMap real_words

    let fake_vectorizer = fitVectorizer fake_count_map
    let real_vectorizer = fitVectorizer real_count_map

    let (CountVectorizer fake_dict fake_counts) = fake_vectorizer
    let (CountVectorizer real_dict real_counts) = real_vectorizer

    print "h"
    putStrLn
        ( case Data.Map.lookup "trump" fake_dict of
            Nothing -> "Trump not found in FAKE"
            Just pos -> show $ fake_counts Data.Vector.! pos
        )

    putStrLn
        ( case Data.Map.lookup "trump" real_dict of
            Nothing -> "Trump not found in REAL"
            Just pos -> show $ real_counts Data.Vector.! pos
        )