module Jak.Content.DoubleSeq where

import Jak.Core

import qualified Data.Sequence as S
import Data.Foldable
import Data.Maybe
import Data.Monoid

makeStrings :: S.Seq (S.Seq Char) -> [String]
makeStrings = toList . fmap toList

data DoubleSeq = DoubleSeq
  { contentSeq :: S.Seq (S.Seq Char)
  , cursorRow :: Int
  , cursorCol :: Int
  , virtualCol :: Int
  } deriving Eq

instance Show DoubleSeq where
  show (DoubleSeq s r c v) = show ((r,c,v), init (unlines (makeStrings s)))

instance Content DoubleSeq where
  -- O(log(r'))
  move (r',c') (DoubleSeq s r c v) =
    let r'' = max 0 (min r' (length s - 1)) -- O(1)
        c'' = max 0 (min c' (length (S.index s r''))) -- O(log(min(r'',length s - r''))
    in DoubleSeq s r'' c'' c''
  -- O(log(r))
  moveUp (DoubleSeq s r c v) = case s S.!? (r - 1) of
    Just s' -> DoubleSeq s (r - 1) (min v (length s')) v
    Nothing -> DoubleSeq s r c v
  -- O(log(r))
  moveDown (DoubleSeq s r c v) = case s S.!? (r + 1) of
    Just s' -> DoubleSeq s (r + 1) (min v (length s')) v
    Nothing -> DoubleSeq s r c v
  -- O(log(r))
  moveLeft crossNewline (DoubleSeq s r c v)
    | c > 0 = DoubleSeq s r (c - 1) (c - 1) -- O(1)
    | not crossNewline = DoubleSeq s r 0 0 -- O(1)
    | otherwise = case s S.!? (r - 1) of -- O(log(r))
        Just s' -> DoubleSeq s (r - 1) (length s') (length s')
        Nothing -> DoubleSeq s 0 0 0
  -- O(log(r))
  moveRight crossNewline (DoubleSeq s r c v)
    | c < length (S.index s r) = DoubleSeq s r (c + 1) (c + 1) -- O(log(r))
    | not crossNewline = DoubleSeq s r c c -- O(1)
    | otherwise = case s S.!? (r + 1) of -- O(log(r))
        Just s' -> DoubleSeq s (r + 1) (min v (length s')) v
        Nothing -> DoubleSeq s r c v
  -- O(log(c+r) + log(2r))
  insert '\n' (DoubleSeq s r c v) = 
    let (a,b) = S.splitAt c (S.index s r) -- O(log(c)log(r))
    in DoubleSeq (S.insertAt r a (S.update r b s)) (r + 1) 0 0 -- O(log(2r))
  -- O(log(c+r))
  insert i (DoubleSeq s r c v) = let s' = (S.adjust' (S.insertAt c i) r s) in DoubleSeq s' r (c + 1) (c + 1)
  -- O(log(r+c)+log(2r))
  delete (DoubleSeq s r c v) = case S.index s r S.!? c of
    Just _ -> let s' = S.adjust' (S.deleteAt c) r s in DoubleSeq s' r c v
    Nothing -> case s S.!? (r + 1) of
      Just s' -> DoubleSeq (S.deleteAt (r+1) (S.adjust' (<> s') r s)) r c v
      Nothing -> DoubleSeq s r c v
  -- O(1)
  backspace (DoubleSeq s 0 0 v) = DoubleSeq s 0 0 0
  -- O(log(r+c)+log(2r))
  backspace i = delete (moveLeft True i)
  -- O(r*c)
  renderContent (DoubleSeq s r c v) = ((c,r), makeStrings s)
  -- O(1)
  emptyContent = DoubleSeq (S.singleton S.empty) 0 0 0
  resize (r,c) = id