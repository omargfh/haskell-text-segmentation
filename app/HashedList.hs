-- | Define a module for a hashed array vector in Haskell for O(1)
-- look up time
module HashedList where

    import qualified Data.Vector as Vector
    import qualified Data.Hashable as Hashable

    import Data.List hiding ( lookup )
    import Data.Char

    import System.IO
    import Data.Functor
    import Control.Applicative

    -- | Creates space in memory for the dictionary

    -- Initiates an empty vector of a certain data type
    -- to be modified with hashed elements
    initHashMem :: Int -> a -> Vector.Vector a
    initHashMem = Vector.replicate

    -- Initiates an empty vector of lists of strings with the required hash size O(n)
    vector :: Vector.Vector [String]
    vector = initHashMem hashSize [""]

    -- | Hashes an element from the dictionary

    hash :: String -> Int
    hash x = mod (abs $ Hashable.hash x) (10 ^ 5)

    -- | Loads dictionary into memory

    -- Default size of the vector, DO NOT OVERRIDE
    hashSize :: Int
    hashSize = 110000

        -- Takes a list and returns a vector O(n)
    toVector :: [a] -> Vector.Vector a
    toVector = Vector.fromList

    -- Loads a list of strings into memory in the form of a vector
    -- where each element is a linked list and its index is the hash of
    -- each element in the list. O(n)
    loadVector :: Vector.Vector String -> Vector.Vector [String]
    loadVector allWords = Vector.accumulate (++) vector (Vector.map (\x -> (hash x, [x])) allWords)

    -- | Deals with checks and lookups

    -- Checks if a certain word is in the given dictionary O(1)
    isWord :: Vector.Vector [String] -> String -> Bool
    isWord v s = s `elem` returnedWords v s

    -- Returns all words at the hash of a certain word in a given dictionary O(1)
    returnedWords :: Vector.Vector [String] -> String -> [String]
    returnedWords v s = v Vector.! hash s
