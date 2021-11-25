import CountVectorizer (flatten, fitVectorizer, encode)

import qualified Data.Vector (Vector, (!))

data DTree = Internal String Float [DTree] | Leaf Float
    deriving (Show, Eq)

data Pair = Pair Int Int
    deriving (Show, Eq)

main :: IO ()
main = do
    raw_fake <- readFile "data/fake_title.txt"
    raw_real <- readFile "data/real_title.txt"

    let fake = lines raw_fake
    let real = lines raw_real

    let fake_words = flatten fake
    let real_words = flatten real

    let fake_vectorizer = fitVectorizer fake_words
    let real_vectorizer = fitVectorizer real_words

    let to_encode = flatten ["woman trump is a woman"]

    let encoded = encode to_encode fake_vectorizer
    print (encoded Data.Vector.! 118) -- should give 2
