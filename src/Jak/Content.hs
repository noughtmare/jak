{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}

module Jak.Content (content) where

import Prelude hiding ((.), id)
import Control.Category

import Jak.Types

import FRP.Yampa

import qualified Data.Sequence as S
import Data.Sequence (Seq (..))
import Data.Monoid ((<>))

content :: Content -> SF (Event EditorEvent, Position) Content
content c0 = accumHoldBy replaceContent c0 . arr (\(e,p) -> e >>= mkReplace p)
  where
    mkReplace p = \case
      EditorInsert c  -> Event (Range p (Size 0 0), [c])
      EditorBackspace -> Event (Range p (Size 1 0), "") -- FIXME
      EditorDelete    -> Event (Range p (Size 1 0), "")
      -- TODO: Make ContentEvent datatype (See the TODO in Cursor.hs)
      _ -> NoEvent

replaceContent :: Content -> (Range, String) -> Content
replaceContent (Content s) (Range pos@(Position _ _) size, as) =
  Content (a `overlap` c `overlap` b)
  where
    (a,b') = splitAtPosition pos s
    b = dropUntilPosition (s2p size) b'
    c = toDoubleSeq as

overlap :: Seq (Seq Char) -> Seq (Seq Char) -> Seq (Seq Char)
overlap (Empty   ) (b       ) = b
overlap (a       ) (Empty   ) = a
overlap (a :|> aa) (bb :<| b) = mconcat [a, S.singleton (aa <> bb), b]

dropUntilPosition :: Position -> Seq (Seq Char) -> Seq (Seq Char)
dropUntilPosition (Position (C c) (R r)) = f . S.drop r
  where
    f Empty = Empty
    f (x :<| xs)
      | c > length x = xs
      | otherwise = S.drop c x :<| xs

splitAtPosition :: Position
                -> Seq (Seq Char)
                -> (Seq (Seq Char), Seq (Seq Char))
splitAtPosition (Position (C col) (R row)) s = case S.splitAt row s of
  (a,b :<| c) -> let (d,e) = S.splitAt col b in (a :|> d, e :<| c)
  a -> a

toDoubleSeq :: String -> Seq (Seq Char)
toDoubleSeq = S.fromList . map S.fromList . lines' where 
  -- Proper lines implementation ;)
  lines' :: String -> [String]
  lines' = uncurry (:) . foldr cons ([], []) where
    cons '\n' ~(x,xs) = ([], x:xs) -- start a new line
    cons c    ~(x,xs) = (c:x, xs)  -- add the character to the current line
