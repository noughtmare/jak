{-# LANGUAGE LambdaCase #-}
module Jak.Content (content) where

import Jak.Types
import Control.FRPNow.Util

import qualified Data.Sequence as S
import Data.Sequence (Seq (..))
import Data.Bifunctor (bimap)
import Control.FRPNow
import Data.Monoid ((<>))

content :: Content
        -> EvStream EditorEvent
        -> Position
        -> EvStream Position
        -> Behavior (EvStream Content)
content a evs pos0 posEs = do
  pos <- fromChanges pos0 posEs
  dpos <- delay evs pos0 pos
  scanlEv replaceContent a
    $ uncurry merge
    $ bimap (f dpos) (f pos)
    $ partitionEs isBefore evs
  where
    f p = filterMapEs (uncurry mkReplace) . (fmap (,) p <@@>)

    isBefore = \case
      EditorInsert _ -> True
      _              -> False

    mkReplace p = \case
      EditorInsert c  -> Just (Range p (Size 0 0), [c])
      EditorBackspace -> Just (Range p (Size 1 0), "")
      EditorDelete    -> Just (Range p (Size 1 0), "")
      _ -> Nothing

replaceContent :: Content -> (Range, String) -> Content
replaceContent (Content s) (Range pos@(Position cc _) size, as) =
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
  (a,b :<| c) -> let (d,e) = S.splitAt col b
                 in (a :|> d, e :<| c)
  a -> a

toDoubleSeq :: String -> Seq (Seq Char)
toDoubleSeq = S.fromList . map S.fromList . lines'
  where
    -- Proper lines implementation ;)
    lines' :: String -> [String]
    lines' = foldr (\a ~(b:bs) -> if a == '\n' then []:b:bs else (a:b):bs) [[]]
