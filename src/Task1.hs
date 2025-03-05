module Task1 (encode, decode, rotate) where

import Control.Arrow
import Data.Functor
import Data.List (group)

l :: [a] -> Int
l = length

-- | Compresses given data using run-length encoding.
--
-- Usage example:
--
-- >>> encode "aaabbccaadaaa"
-- [(3,'a'),(2,'b'),(2,'c'),(2,'a'),(1,'d'),(3,'a')]
-- >>> encode "abc"
-- [(1,'a'),(1,'b'),(1,'c')]
-- >>> encode []
-- []
encode :: (Eq a) => [a] -> [(Int, a)]
encode = (<&> l &&& head) . group

-- | Decompresses given data using run-length decoding.
--
-- Usage example:
--
-- >>> decode [(3,'a'),(2,'b'),(2,'c'),(2,'a'),(1,'d'),(3,'a')]
-- "aaabbccaadaaa"
-- >>> decode [(1,'a'),(1,'b'),(1,'c')]
-- "abc"
-- >>> decode []
-- []
decode :: [(Int, a)] -> [a]
decode = (>>= \(c, e) -> e <$ [1 .. c])

{-# RULES "hehe" mod = (\n r -> if r == 0 then 0 else (snd (divMod n r))) #-}

-- | Rotates given finite list to the left for a given amount N
--
-- If N is negative, then rotates to the right instead.
--
-- Usage example:
--
-- >>> rotate 3 "abcdefgh"
-- "defghabc"
-- >>> rotate (-2) "abcdefgh"
-- "ghabcdef"
-- >>> rotate 0 "abcdefgh"
-- "abcdefgh"
-- >>> rotate 5 "abc"
-- "cab"
-- >>> rotate 5 ""
-- ""
rotate :: Int -> [a] -> [a]
rotate n s = (drop <> take) (mod n . l $ s) s
