-- | Defines module for common words
-- to intellegintely process a string
-- for word segmentation
module CommonWords where

-- A list of allowed one-letter words
singletonWords :: [String]
singletonWords = ["a", "o", "i"]

-- A list of vowels in the English language
vowels :: [String]
vowels = ["a", "e", "i", "o", "u"]
    
-- A group of lists of the common words in each letter
-- to override greedy segmentations
words1 :: [[Char]]
words1 = ["about", "after", "also", "all", "and", "any", "as", "at"]
words2 :: [[Char]]
words2 = ["back", "because", "but", "be", "by"]
words3 :: [[Char]]
words3 = ["could", "come", "can"]
words4 :: [[Char]]
words4 = ["day", "do"]
words5 :: [[Char]]
words5 = ["even"]
words6 :: [[Char]]
words6 = ["first", "from", "for"]
words7 :: [[Char]]
words7 = ["good", "give", "get", "go"]
words8 :: [[Char]]
words8 = ["have", "him", "her", "has", "his", "how", "he"]
words9 :: [[Char]]
words9 = ["into", "its", "in", "it", "is", "if"]
words10 :: [[Char]]
words10 = ["just"]
words11 :: [[Char]]
words11 = ["know"]
words12 :: [[Char]]
words12 = ["like", "look"]
words13 :: [[Char]]
words13 = ["make", "most", "me", "my"]
words14 :: [[Char]]
words14 = ["new", "now", "not", "no"]
words15 :: [[Char]]
words15 = ["other", "over", "only", "one", "our", "out", "on", "of", "or"]
words16 :: [[Char]]
words16 = ["people"]
words17 :: [[Char]]
words17 = []
words18 :: [[Char]]
words18 = []
words19 :: [[Char]]
words19 = ["some", "say", "see", "she", "so"]
words20 :: [[Char]]
words20 = ["think", "take", "than", "that", "them", "then", "than", "they", "this", "two", "to"]
words21 :: [[Char]]
words21 = ["use", "up", "us"]
words22 :: [[Char]]
words22 = []
words23 :: [[Char]]
words23 = ["which", "would", "were", "with", "work", "what", "want", "was", "way", "who", "well", "we"]
words24 :: [[Char]]
words24 = []
words25 :: [[Char]]
words25 = ["year", "your", "you"]
words26 :: [[Char]]
words26 = []

-- A list of all the lists of common words in alphabetical order
commonWords :: [[[Char]]]
commonWords = [words1, words2, words3, words4, words5, words6, words7, words8, words9, words10, words11, words12, words13, words14,
                   words15, words16, words17, words18, words19, words20, words21, words22, words23, words24, words25, words26]