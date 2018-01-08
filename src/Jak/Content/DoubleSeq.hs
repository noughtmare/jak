{-# LANGUAGE TypeOperators #-}
module Jak.Content.DoubleSeq where

import Jak.Core

import qualified Data.Sequence as S
import Data.Foldable
import Data.Maybe
import Data.Monoid

makeStrings :: S.Seq (S.Seq Char) -> [String]
makeStrings = toList . fmap toList

data MyCursor a = MyCursor
  { _cursorGetPos    :: a -> (Int,Int)
  , _cursorMoveAbs   :: (Int,Int) -> a -> a
  , _cursorMoveUp    :: a -> a
  , _cursorMoveDown  :: a -> a
  , _cursorMoveRight :: a -> a
  , _cursorMoveLeft  :: a -> a
  }

data MyContent a = MyContent
  { _contentShape :: a -> S.Seq Int
  , _contentReplace :: ((Int,Int),(Int,Int)) -> String -> a -> a
  }

-- Note: I haven't tested this, so assume there is a bug.
replace :: ((Int,Int),(Int,Int)) -> String -> S.Seq (S.Seq Char) -> S.Seq (S.Seq Char)
replace range as s = replace' range (map S.fromList (lines' as)) s
  where
    replace' :: ((Int,Int),(Int,Int)) -> [S.Seq Char] -> S.Seq (S.Seq Char) -> S.Seq (S.Seq Char)
    -- Insert a single line... (a line without newline)
    -- ... in a single line.
    replace' ((c,r),(w,0)) [as] s
      = S.adjust' (\line -> let (a,b) = S.splitAt c line in a <> as <> S.drop w b) r s
    -- ... in multiple lines.
    replace' ((c,r),(w,h)) [as] s
      = case s S.!? (r + h) of
          Just b -> S.adjust' (\line -> S.take c line <> as <> b) r s
          Nothing -> S.adjust' (\line -> S.take c line <> as) r s
    -- Insert multiple lines...
    -- ... in a single line.
    replace' ((c,r),(w,0)) (as:ass) s
      = case s S.!? r of
          Just a' -> let (a,b') = S.splitAt c a'
                         b = S.drop w b
                         (aa,bb') = S.splitAt r s
                         bb = S.drop 1 bb'
                     in mconcat [aa,S.fromList (a <> as : init ass ++ [last ass <> b]),bb]
          Nothing -> s
    -- ... in multiple lines.
    replace' ((c,r),(w,h)) (as:ass) s 
      = case s S.!? r of
          Just a' -> let a = S.take c a'
                         b = S.drop (c+w) (fromMaybe S.empty (s S.!? (r + h)))
                         (aa,bb') = S.splitAt r s
                         bb = S.drop (h + 1) bb'
                     in mconcat [aa,S.fromList (a <> as : init ass ++ [last ass <> b]),bb]
          Nothing -> s

    -- Proper lines implementation ;)
    lines' = foldr (\a (b:bs) -> if a == '\n' then []:b:bs else (a:b):bs) [[]]

-- TODO: finish this
cacheShape :: MyContent a -> MyContent (a, S.Seq Int)
cacheShape (MyContent _ replace) = MyContent
  { _contentShape = snd
  , _contentReplace = \range@((_,r),(_,h)) t (c,s) -> 
      let s' = replace range t s 
      in (s', S.take r c <> S.fromList (map length )

doubleSeqContent :: MyContent (S.Seq (S.Seq Char))
doubleSeqContent = MyContent
  { _contentShape = fmap length
  , _contentReplace = replace
  }

data DoubleSeq = DoubleSeq
  { contentSeq     :: S.Seq (S.Seq Char)
  , cursorRow      :: Int
  , cursorCol      :: Int
  , virtualCol     :: Int
  , viewportOrigin :: Int
  , viewportSize   :: (Int, Int)
  } deriving Eq

instance Show DoubleSeq where
  show (DoubleSeq s r c v o (h,w)) = show ((r,c,v,o,(h,w)))

updateViewport :: DoubleSeq -> DoubleSeq
updateViewport (DoubleSeq s r c v o (h,w) {-ic-}) = DoubleSeq s r c v o' (h,w) {-ic-}
  where o' | r < o = r
           | r >= o + h = r - (h - 1)
           | otherwise = o

instance Content DoubleSeq where
  -- O(log(r'))
  move (r',c') (DoubleSeq s r c v o (h,w)) = updateViewport $
    let r'' = max 0 (min r' (length s - 1)) -- O(1)
        c'' = max 0 (min c' (length (S.index s r''))) -- O(log(min(r'',length s - r''))
    in DoubleSeq s r'' c'' c'' o (h,w)
  -- O(log(r))
  moveUp ds@(DoubleSeq s r c v o (h,w)) = updateViewport $ case s S.!? (r - 1) of
    Just s' -> DoubleSeq s (r - 1) (min v (length s')) v o (h,w)
    Nothing -> ds
  -- O(log(r))
  moveDown ds@(DoubleSeq s r c v o (h,w)) = updateViewport $ case s S.!? (r + 1) of
    Just s' -> DoubleSeq s (r + 1) (min v (length s')) v o (h,w)
    Nothing -> ds
  -- O(log(r))
  moveLeft crossNewline (DoubleSeq s r c v o (h,w))
    | c > 0 = DoubleSeq s r (c - 1) (c - 1) o (h,w) -- O(1)
    | not crossNewline = DoubleSeq s r 0 0 o (h,w) -- O(1)
    | otherwise = case s S.!? (r - 1) of -- O(log(r))
        Just s' -> updateViewport $ DoubleSeq s (r - 1) (length s') (length s') o (h,w)
        Nothing -> DoubleSeq s 0 0 0 o (h,w)
  -- O(log(r))
  moveRight crossNewline (DoubleSeq s r c v o (h,w))
    | c < length (S.index s r) = DoubleSeq s r (c + 1) (c + 1) o (h,w) -- O(log(r))
    | not crossNewline = DoubleSeq s r c c o (h,w) -- O(1)
    | otherwise = case s S.!? (r + 1) of -- O(log(r))
        Just s' -> updateViewport $ DoubleSeq s (r + 1) (min v (length s')) v o (h,w)
        Nothing -> DoubleSeq s r c v o (h,w)
  -- O(log(c+r) + log(2r))
  insert '\n' (DoubleSeq s r c v o (h,w)) = 
    let (a,b) = S.splitAt c (S.index s r) -- O(log(c)log(r))
    in updateViewport $ DoubleSeq (S.insertAt r a (S.update r b s)) (r + 1) 0 0 o (h,w) -- O(log(2r))
  -- O(log(c+r))
  insert i (DoubleSeq s r c v o (h,w)) = let s' = (S.adjust' (S.insertAt c i) r s) in DoubleSeq s' r (c + 1) (c + 1) o (h,w)
  -- O(log(r+c)+log(2r))
  delete (DoubleSeq s r c v o (h,w)) = case S.index s r S.!? c of
    Just _ -> let s' = S.adjust' (S.deleteAt c) r s in DoubleSeq s' r c v o (h,w)
    Nothing -> case s S.!? (r + 1) of
      Just s' -> updateViewport $ DoubleSeq (S.deleteAt (r+1) (S.adjust' (<> s') r s)) r c v o (h,w)
      Nothing -> DoubleSeq s r c v o (h,w)
  -- O(1)
  backspace (DoubleSeq s 0 0 v o (h,w)) = DoubleSeq s 0 0 0 o (h,w)
  -- O(log(r+c)+log(2r))
  backspace i = delete (moveLeft True i)
  -- O(r*c)
  renderContent (DoubleSeq s r c v o (h,w)) = ((c,r - o), makeStrings (fmap (S.take w) (S.take h (S.drop o s))))
  -- O(1)
  emptyContent (h,w) = DoubleSeq (S.singleton S.empty) 0 0 0 0 (h,w)
  resize (h,w) (DoubleSeq s r c v o _) = updateViewport (DoubleSeq s r c v o (h,w))
