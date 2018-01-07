module Jak.Content.SingleSeq where

import Jak.Core
import Data.Monoid.Nth
import Data.Monoid
import qualified Data.Sequence as S
import Data.Foldable
import Data.Maybe

data SingleSeq = SingleSeq
  { singleSeq :: S.Seq Char
  , singleSeqCursorPos :: Int
  , singleSeqVirtualCol :: Int
  } deriving Eq

instance Show SingleSeq where
  show (SingleSeq s i v) = show ((i,v),toList s)

getCol :: Int -> S.Seq Char -> Int
getCol i s = case S.elemIndexR '\n' (S.take i s) of
  Just i' -> i - i' - 1
  Nothing -> i
--  fromMaybe (i - 1) (S.elemIndexR '\n' (S.take (i - 1) s))

getColInRow :: Int -> Int -> Int -> Int
getColInRow p1 p2 v = (p1 + 1 + min v (p2 - p1 - 1))

instance Content SingleSeq where
  move (r,c) (SingleSeq s i v) = (\(p1,p2) -> SingleSeq s (getColInRow p1 p2 c) c) $ case drop r (S.elemIndicesL '\n' s) of
    []        -> (fromMaybe (-1) (S.elemIndexR '\n' s), length s)
    [p1]      -> (p1, length s)
    (p1:p2:_) -> (p1,p2)

  moveUp (SingleSeq s i v) = case S.elemIndicesR '\n' (S.take i s) of
    []        -> SingleSeq s i v
    [p2]      -> SingleSeq s (min v p2) v
    (p2:p1:_) -> SingleSeq s (getColInRow p1 p2 v) v

  moveDown (SingleSeq s i v) = case map (i +) (S.elemIndicesL '\n' (S.drop i s)) of
    []        -> SingleSeq s i v
    [p1]      -> SingleSeq s (getColInRow p1 (length s) v) v
    (p1:p2:_) -> SingleSeq s (getColInRow p1 p2 v) v

  moveLeft ignoreNewline (SingleSeq s i v) = case s S.!? (i - 1) of
    Just '\n' | ignoreNewline -> SingleSeq s (i - 1) (getCol (i - 1) s)
              | otherwise     -> SingleSeq s i v
    Just _                    -> SingleSeq s (i - 1) (v - 1)
    Nothing                   -> SingleSeq s i v

  moveRight ignoreNewline (SingleSeq s i v) = case s S.!? i of
    Just '\n' | ignoreNewline -> SingleSeq s (i + 1) 0 
              | otherwise     -> SingleSeq s i v
    Just _                    -> SingleSeq s (i + 1) (v + 1)
    Nothing                   -> SingleSeq s i v

  insert '\n' (SingleSeq s i v) = SingleSeq (S.insertAt i '\n' s) (i + 1) 0
  insert c    (SingleSeq s i v) = SingleSeq (S.insertAt i c    s) (i + 1) (v + 1)

  delete    (SingleSeq s i v) = SingleSeq (S.deleteAt i s) i v

  backspace (SingleSeq s 0 v) = SingleSeq s 0 v
  backspace s                 = delete (moveLeft True s)

  renderContent (SingleSeq s i v) = 
    ( ( i - fromMaybe 0 ((1 +) <$> S.elemIndexR '\n' (S.take i s))
      , length (S.filter (== '\n') (S.take i s))
      )
    , lines (toList s))

  emptyContent = SingleSeq mempty 0 0

safeLast, safeHead :: Foldable t => t a -> Maybe a
safeLast xs = getLast (foldMap pure xs)
safeHead xs = getFirst (foldMap pure xs)

safeIndex :: Foldable f => f a -> Int -> Maybe a
safeIndex xs = getNth (foldMap nth xs) 