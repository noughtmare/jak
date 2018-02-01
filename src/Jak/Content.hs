{-# LANGUAGE LambdaCase #-}
module Jak.Content where

import Jak.Types
import qualified Data.Sequence as S
import Data.Bifunctor (bimap)
import Control.FRPNow
import Control.FRPNow.Util
import Data.Monoid ((<>))

contentBehavior :: Content
                -> EvStream EditorEvent
                -> EvStream Position
                -> Position
                -> Behavior (EvStream Content)
contentBehavior a evs posEs pos0 = do
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
replaceContent (Content s) (Range pos size, as) =
   let (a,b') = splitAtPosition pos s
       b = dropUntilPosition (s2p size) b'
   in Content (a `overlap` toDoubleSeq as `overlap` b)

overlap (S.Empty     ) (b           ) = b
overlap (a           ) (S.Empty     ) = a
overlap ((a S.:|> aa)) ((bb S.:<| b)) = (mconcat [a, S.singleton (aa <> bb), b])

dropUntilPosition :: Position -> S.Seq (S.Seq Char) -> S.Seq (S.Seq Char)
dropUntilPosition (Position (C c) (R r)) = f . S.drop r
  where
    f S.Empty = S.Empty
    f (x S.:<| xs) = S.drop c x S.:<| xs

splitAtPosition :: Position
                -> S.Seq (S.Seq Char)
                -> (S.Seq (S.Seq Char), S.Seq (S.Seq Char))
splitAtPosition (Position (C col) (R row)) s = case S.splitAt row s of
  (a,b S.:<| c) -> let (d,e) = S.splitAt col b in (a S.:|> d, e S.:<| c)
  (a,S.Empty) -> (a,S.Empty)

toDoubleSeq :: String -> S.Seq (S.Seq Char)
toDoubleSeq = S.fromList . map S.fromList . lines'
  where
    -- Proper lines implementation ;)
    lines' :: String -> [String]
    lines' = foldr (\a ~(b:bs) -> if a == '\n' then []:b:bs else (a:b):bs) [[]]
