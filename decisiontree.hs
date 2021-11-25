import qualified Data.Map (Map, empty, fromList, insert, lookup, size)

data DTree = Internal String Float [DTree] | Leaf Float
    deriving (Show, Eq)

type Dictionary = Data.Map.Map String Int

data CountVectorizer = CountVectorizer Dictionary [Int]
    deriving (Show, Eq)

addToVectorizer :: CountVectorizer -> String -> CountVectorizer
addToVectorizer (CountVectorizer dict counts) word = case Data.Map.lookup word dict of
    Nothing ->
        -- add word to dictionary with value of the length of counts
        CountVectorizer (Data.Map.insert word (length counts) dict) (counts ++ [1])
    Just pos ->
        -- increment the value of counts at pos
        CountVectorizer dict (take pos counts ++ [counts !! pos + 1] ++ drop (pos + 1) counts)

vectorize :: [String] -> CountVectorizer
vectorize = foldl addToVectorizer (CountVectorizer Data.Map.empty [])

-- takes a list of sentences and returns a list of words
flatten :: [String] -> [String]
flatten sentences = foldr ((++) . words) [] sentences

main :: IO ()
main = do
    raw_fake <- readFile "data/fake_title.txt"
    raw_real <- readFile "data/real_title.txt"
    let fake = lines raw_fake
    let real = lines raw_real

    let fake_words = flatten fake
    let real_words = flatten real

    let fake_vectorizer = vectorize fake_words
    putStrLn "vectorized fake"
    let real_vectorizer = vectorize real_words
    putStrLn "vectorized real"

    let (CountVectorizer fake_dict fake_counts) = fake_vectorizer
    let (CountVectorizer real_dict real_counts) = real_vectorizer

    putStrLn
        ( case (Data.Map.lookup "trump" fake_dict) of
            Nothing -> "Trump not found in FAKE"
            Just pos -> show (fake_counts !! pos)
        )

    putStrLn
        ( case (Data.Map.lookup "trump" real_dict) of
            Nothing -> "Trump not found in REAL"
            Just pos -> show (real_counts !! pos)
        )