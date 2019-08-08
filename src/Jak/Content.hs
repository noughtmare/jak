{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}

module Jak.Content (Content, ContentEvent (..), content, emptyContent) where

import Prelude hiding ((.), id)
import Control.Category

import Jak.Types

import FRP.Yampa

import qualified Data.Sequence as S
import Data.Sequence (Seq (..))
import Data.Foldable (toList)
import Data.Monoid ((<>))

newtype Content = Content
  { contentSeq :: S.Seq (S.Seq Char)
  }

instance ViewContent Content where
  viewContent (Content con) (Position vc vr, Size vw vh) =
    map (toList . S.take (fromIntegral vw) . S.drop (fromIntegral vc))
      $ toList $ S.take (fromIntegral vh) $ S.drop (fromIntegral vr) con

instance HasShape Content where
  projectShape = Shape . fmap S.length . contentSeq

emptyContent :: Content
emptyContent = Content S.Empty

data ContentEvent
  = ContentInsert !Char
  | ContentBackspace
  | ContentDelete

content :: Content -> SF (Event ContentEvent, Position) Content
content c0 = accumHoldBy replaceContent c0 . arr (\(e,p) -> e >>= mkReplace p)
  where
    mkReplace p = \case
      ContentInsert c  -> Event (Range p (Size 0 0), [c])
      ContentBackspace -> Event (Range p (Size 1 0), "") -- FIXME
      ContentDelete    -> Event (Range p (Size 1 0), "")

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
