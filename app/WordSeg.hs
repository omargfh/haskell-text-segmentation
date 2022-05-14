-- Defines module WordSeg for word segmentation functions
module WordSeg where

    import qualified Data.Vector as Vector
    import qualified Data.Char   as Char

    import CommonWords
    import HashedList


    -- | Deals with one-letter words and maximum length of a sentence

    -- Define maximum word length to be the length of the
    -- largest word in the dictionary file
    maxWordLength :: Int
    maxWordLength = 24

    -- Checks if a certain string is an allowed single-character string,
    -- returns true if the string is longer than one character
    isLegalSingelton :: String -> Bool
    isLegalSingelton str = (length str == 1 && str `elem` singletonWords) || (length str > 1)

    -- Checks if a string is both an allowed word and
    -- either an allowed single-character word or longer
    -- than a single character
    isLegalWord :: Vector.Vector [String] -> String -> Bool
    isLegalWord dict str = isWord dict str && isLegalSingelton str

    -- | Deters ambiguity around common words with close spelling

    -- Checks if the indefinite article is followed
    -- by a vowel or a constant and returns whether
    -- it is likely to be lingustically correct or not
    isIndefArticle :: String -> String -> Bool
    isIndefArticle article (a:as) =
        case (article, [a] `elem` vowels) of
            ("a", True)   -> True
            ("an", False) -> True
            (_, _)        -> False
    isIndefArticle article _      = False

    -- Checks the definite article 'the' for commong confusions with
    -- their, these, there, and other words
    isDefArticle :: Vector.Vector [String] -> String -> String -> Bool
    isDefArticle dict article (a:b:as)
        | article ++ [a] ++ [b] == "these"
          && predicate                         = False
        | article ++ [a] ++ [b] == "their"
          && predicate                         = False
        | article ++ [a] ++ [b] == "there"
          && predicate                         = False
        | article == "the"                     = True
        | otherwise                            = False
        where rest      = fst (traverseString dict as)
              predicate = length rest > 1 && rest `notElem` singletonWords
    isDefArticle dict article _                = True

    -- Checks if a string begins with a given a word and is followed
    -- by at least one lingustically-correct word. The second condition
    -- can be overriden by providing (True) as a bool value. The function
    -- takes a dictionary of words, a string with the text, the word
    -- to be checked, and the overriding bool.
    beginsWith :: Vector.Vector [String] -> String -> String -> Bool -> Bool
    beginsWith dict sentence prefix bool =
        case splitAt (length prefix) sentence of
            (pfix, as) -> (isLegalWord dict a || bool) && (isLegalSingelton b || bool) && pfix == prefix where
                (a, b) = traverseString dict as

    -- Splits a string at its beginning if beginsWith returns true.
    splitAtBeginning :: Vector.Vector [String] -> String -> [String] -> Bool -> String
    splitAtBeginning dict [] _ b = ""
    splitAtBeginning dict s [] b = s
    splitAtBeginning dict s (c:cs) b
        | beginsWith dict s c b  = c `concatString` rest
        | otherwise           = splitAtBeginning dict s cs b
        where rest   = drop (length c) s

    -- Takes a string and checks against the common words from the Common Words
    -- library for the first letter of the string and splits and concatenates
    -- accordingly.
    splitByCommonWords :: Vector.Vector [String] -> String -> Bool -> String
    splitByCommonWords dict s@(a:as) = splitAtBeginning dict s c
        where c                        = commonWords !! (fromEnum a - 97)

    -- | Deals with intelligent segmentation
    
    -- Takes a string and checks it against common confusions for "the", "an", "a",
    -- and the common words from the common words library. Otherwise it returns
    -- the original string.
    segmentFromCommonWords :: Vector.Vector [String] -> String -> Bool -> String
    segmentFromCommonWords dict "" b = ""
    segmentFromCommonWords dict i@('t':'h':'e':xs) b
        | isDefArticle dict "the" xs && beginsWith dict i "the" b = "the" `concatString` xs
    segmentFromCommonWords dict i@(x:xs) b
        | splitByCommonWords dict i b /= i                       = splitByCommonWords dict i b
    segmentFromCommonWords dict i@('a':x:xs) b
        | isIndefArticle "an" xs && beginsWith dict i "an" b      = "an" `concatString` xs
        | isIndefArticle "a"  xs && beginsWith dict i "a"  b      = "a" `concatString` (x:xs)
    segmentFromCommonWords _ i _                                  = i

    -- Takes a dictionary and a string and applies smartTraverse to it.
    -- The function splits the string at the maximum word length, applies
    -- the traverse function, and recursively segments the remaining output
    -- of traverse after concatenating it with the rest of the string.
    smartSegment :: Vector.Vector [String] -> String -> String
    smartSegment dict ""           = ""
    smartSegment dict i            = stripString $ word `concatString` smartSegment dict (rest ++ b)
        where (a, b)               = splitAt maxWordLength i
              (word, rest)         = smartTraverse dict a

    -- Takes a string and returns a tuple of two elements: the first is a
    -- single word from the beginning of the string, and the second is the
    -- remaining string. The function starts by taking the whole string,
    -- checks if it is a word. If not, it drops one letter and recursively
    -- supplies the result to itself. If the length of the string is equal
    -- to or below 4 characters, it checks against common words instead.
    smartTraverse ::  Vector.Vector [String] -> String -> (String, String)
    smartTraverse dict []         = ([], [])
    smartTraverse dict [a]        = ([a], [])
    smartTraverse dict i
        | let s = words (segmentFromCommonWords dict i True)
          , length i <= 4
          && isWord dict (head s)  = if length s == 1 then (head s, "") else (head s, last s) 
        | isWord dict i            = (i, [])
        | otherwise                = (res, rest ++ [b])
        where (a, b)      = (init i, last i)
              (res, rest) = smartTraverse dict a

    -- | Concatenates and strips a String
    
    -- Concatenates two strings with a space in the middle
    concatString :: String -> String -> String
    concatString a b = a ++ " "  ++ b

    -- Strips a string from all the spaces at the end
    stripString :: String -> String
    stripString str =
        case reverse str of
            (' ':as) -> stripString (reverse as)
            as       -> reverse as

    -- Basic implementation of traverse string. This implementation is identical
    -- to smartTraverse, except that it does not check for the length of the test
    -- string.
    traverseString :: Vector.Vector [String] -> String -> (String, String)
    traverseString dict []         = ([], [])
    traverseString dict [a]        = ([a], [])
    traverseString dict i
        | isWord dict i            = (i, [])
        | otherwise                = (res, rest ++ [b])
        where (a, b)      = (init i, last i)
              (res, rest) = traverseString dict a
