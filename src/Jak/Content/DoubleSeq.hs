{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
module Jak.Content.DoubleSeq where

-- |
-- The changes in this file have taken me about a week to think through and
-- implement. I will list some of the most important decisions here at the
-- top of the file.
--
-- === Which FRP library should I use?
--
-- I chose to use frpnow mainly because it promises an efficient implementation
-- and it doesn't have many extraneous features which I probably won't need and
-- will only get in the way of learning how to use it.
--
-- The main alternatives for me would be Yampa, but I didn't want to learn how
-- to use the arrow interface. Or maybe reflex when I'm more confortable with
-- FRP.
--
-- === How to make sure I don't confuse the row, column, width or height?
--
-- I am using explicit data types for each different type of integer. I hope
-- this will prevent most mistakes that can happen.
--
-- The main disadvantage of this is that conversions between the types need
-- to be explicitely specified by the programmer, but I personally really
-- like it when all the types logically fit together.
--
-- === How to make the code more modular?
--
-- I have decided to split the viewport, cursor and content. This should
-- improve the readability and modularity of the code. A disadvantage is
-- that some internal information is lost so some functions may have
-- worse performance, but it shouldn't influence the worst case time
-- complexity.
--
-- When implementing this I really noticed that the code became much more
-- readable.
--
-- TODO:
--   - Split this module into multiple modules, one for each section.

import Control.Lens
import Control.Applicative
import Control.FRPNow
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as S
import qualified Jak.Core as J
import Control.Monad.Fix
import Data.Bifunctor

--------------------------------------------------------------------------------
-- FRP Utils
--------------------------------------------------------------------------------

partitionEs :: (a -> Bool) -> EvStream a -> (EvStream a, EvStream a)
partitionEs goesLeft evs = (filterEs goesLeft evs, filterEs (not . goesLeft) evs)

fromUpdates :: a -> EvStream (a -> a) -> Behavior (EvStream a)
fromUpdates a evs = scanlEv (flip id) a evs

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Strong types!
-- https://www.fpcomplete.com/blog/2018/01/weakly-typed-haskell

-- FIXME:
-- Instead of having all these awkward conversion functions
-- there should be functions that operate directly on these
-- strong types.
s2p :: Size -> Position
s2p (Size w h) = Position (w2c w) (h2r h)

p2s :: Position -> Size
p2s (Position c r) = Size (c2w c) (r2h r)

-- NOTE: Linear.Affine from the linear package could help with
-- type safety and usability, but I am too lazy to implement it now.
-- (that will also fix the FIXME above)
newtype Width = W Int
  deriving (Eq,Ord,Show,Read,Enum,Num,Real,Bounded,Integral)

w2c :: Width -> Column
w2c = fromIntegral

newtype Height = H Int
  deriving (Eq,Ord,Show,Read,Enum,Num,Real,Bounded,Integral)

h2r :: Height -> Row
h2r = fromIntegral

newtype Row = R Int
  deriving (Eq,Ord,Show,Read,Enum,Num,Real,Bounded,Integral)

r2h :: Row -> Height
r2h = fromIntegral

newtype Column = C Int
  deriving (Eq,Ord,Show,Read,Enum,Num,Real,Bounded,Integral)

c2w :: Column -> Width
c2w = fromIntegral

data Direction = North | East | South | West
  deriving (Show, Eq)

data Range = Range !Position !Size
  deriving (Show, Eq)

data Size = Size !Width !Height
  deriving (Show, Eq)

data Position = Position !Column !Row
  deriving (Show, Eq)

instance Ord Position where
  compare (Position c r) (Position c' r') = case compare r r' of
    LT -> LT
    GT -> GT
    EQ -> compare c c'

--------------------------------------------------------------------------------
-- Viewport
--------------------------------------------------------------------------------

-- The viewport keeps track of the part of the content that is currently
-- visible.

data Viewport = Viewport
  { _viewportPosition :: !Position
  , _viewportSize     :: !Size
  }
viewportPosition :: Lens' Viewport Position
viewportPosition = lens _viewportPosition (\x y -> x { _viewportPosition = y })
viewportSize :: Lens' Viewport Size
viewportSize     = lens _viewportSize     (\x y -> x { _viewportSize     = y })

data ViewportEvent
  = Resize !Size
  | Moved !Position

emptyViewport :: Size -> Viewport
emptyViewport = Viewport (Position 0 0)

viewportBehavior :: Viewport
                 -> EvStream Cursor
                 -> EvStream Size
                 -> Behavior (EvStream Viewport)
viewportBehavior s cur size = scanlEv scrollViewport s viewportEvs
  where
    viewportEvs = fmap Resize size `merge` fmap (Moved . _cursorPos) cur

scrollViewport :: Viewport -> ViewportEvent -> Viewport
scrollViewport (Viewport (Position vc vr) size@(Size w h)) (Moved (Position cc cr))
  = let c | cc < vc          = cc
          | cc >= vc + w2c w = cc - w2c w + 1
          | otherwise        = vc
        r | cr < vr          = cr
          | cr >= vr + h2r h = cr - h2r h + 1
          | otherwise        = vr
    in  Viewport (Position c r) size
scrollViewport (Viewport pos _) (Resize size) = Viewport pos size

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

-- | This is a type that holds values that are used when moving up or down to
-- memorize the column offset.
newtype VirtualColumn = V Int
  deriving (Eq,Ord,Show,Read,Enum,Num,Real,Bounded,Integral)

v2c :: VirtualColumn -> Column
v2c = fromIntegral

c2v :: Column -> VirtualColumn
c2v = fromIntegral

data Cursor = Cursor
  { _cursorPos :: !Position
  , _cursorVirtCol :: !VirtualColumn
  } deriving Eq
cursorPos :: Lens' Cursor Position
cursorPos     = lens _cursorPos     (\x y -> x { _cursorPos     = y })
cursorVirtCol :: Lens' Cursor VirtualColumn
cursorVirtCol = lens _cursorVirtCol (\x y -> x { _cursorVirtCol = y })

data CursorEvent
  = Move !Direction
  | MoveAbs !Position
  | MoveInsert
  | MoveBackspace

newtype Shape = Shape (S.Seq Int)

emptyCursor :: Cursor
emptyCursor = Cursor (Position 0 0) 0

cursorBehavior :: Cursor
               -> EvStream CursorEvent
               -> EvStream Shape
               -> Shape
               -> Behavior (EvStream Cursor)
cursorBehavior a evs shapeEs shape0 = do
  shape  <- fromChanges shape0 shapeEs
  dshape <- delay evs shape0 shape
  scanlEv moveCursor a
    $ uncurry merge
    $ bimap (f dshape) (f shape)
    $ partitionEs isBefore evs
  where
    f s = (fmap (,) s <@@>)
    isBefore = \case
      MoveInsert -> False
      _ -> True

moveCursor :: Cursor -> (Shape,CursorEvent) -> Cursor
moveCursor (Cursor (Position c r) v) (Shape shape, move) =
  case move of
    Move North    -> clampCol $ Position (v2c v) (max 0 (r - 1))
    Move East     -> mkCursor $ Position (min lineLength (c + 1)) r
    Move South    -> clampCol $ Position (v2c v) (min (R (length shape - 1)) (r + 1))
    Move West     -> mkCursor $ Position (max 0 (c - 1)) r
    MoveAbs p     -> clampCol (clampRow p)
    MoveBackspace
      | c > 0 -> mkCursor $ Position (c - 1) r
      | r > 0 -> mkCursor $ Position (C (S.index shape (fromIntegral (r - 1)))) (r - 1)
      | otherwise -> mkCursor $ Position 0 0
    MoveInsert
      | c < lineLength -> mkCursor $ Position (c + 1) r
      | otherwise      -> mkCursor $ Position 0 (r + 1)
  where
    clampCol (Position cc rr) = Cursor (Position (max 0 (min cc (C (S.index shape (fromIntegral rr))))) rr) (c2v cc)
    clampRow (Position cc rr) = Position cc (min (R (length shape - 1)) (max 0 rr))
    lineLength = C $ S.index shape (fromIntegral r)
    mkCursor p@(Position c _) = Cursor p (c2v c)

--------------------------------------------------------------------------------
-- Content (the actual DoubleSeq)
--------------------------------------------------------------------------------

newtype Content = Content
  { _contentSeq :: S.Seq (S.Seq Char)
  }
contentSeq :: Lens' Content (S.Seq (S.Seq Char))
contentSeq = lens _contentSeq (const Content)

-- virtual getter
contentShape :: Getter Content Shape
contentShape f = contramap s2a . f . s2a
  where
    s2a = Shape . fmap length . view contentSeq

emptyContent :: Content
emptyContent = Content S.empty

overlap (S.Empty     ) (b           ) = b
overlap (a           ) (S.Empty     ) = a
overlap ((a S.:|> aa)) ((bb S.:<| b)) = (mconcat [a, S.singleton (aa <> bb), b])

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

--------------------------------------------------------------------------------
-- Editor
--------------------------------------------------------------------------------

data Editor = Editor
  { _editorViewport :: !Viewport
  , _editorCursor   :: !Cursor
  , _editorContent  :: !Content
  }
editorViewport :: Lens' Editor Viewport
editorViewport = lens _editorViewport (\x y -> x { _editorViewport = y })
editorCursor :: Lens' Editor Cursor
editorCursor   = lens _editorCursor   (\x y -> x { _editorCursor   = y })
editorContent :: Lens' Editor Content
editorContent  = lens _editorContent  (\x y -> x { _editorContent  = y })

data EditorEvent
  = EditorInsert    !Char
  | EditorBackspace
  | EditorDelete
  | EditorMoveDir   !Direction
  | EditorMoveAbs   !Position
  | EditorResize    !Size

emptyEditor :: Size -> Editor
emptyEditor size = Editor (emptyViewport size) emptyCursor emptyContent

editorBehavior :: Editor
               -> EvStream EditorEvent
               -> Behavior (EvStream Editor)
editorBehavior editor@(Editor vpt0 cur0 con0) evs = mdo
  cur <- cursorBehavior
           cur0
           (moveEvs evs)
           (fmap (view contentShape) con)
           (view contentShape con0)
  con <- contentBehavior
           con0
           evs
           (fmap (view cursorPos) cur)
           (view cursorPos cur0)
  vpt <- viewportBehavior
           vpt0
           cur
           (sizeEvs evs)
  fromUpdates editor
    (mconcat [ fmap (set editorViewport) vpt
             , fmap (set editorContent ) con
             , fmap (set editorCursor  ) cur ])
  where
    sizeEvs :: EvStream EditorEvent -> EvStream Size
    sizeEvs = filterMapEs $ \case
      EditorResize s -> Just s
      _ -> Nothing

    moveEvs :: EvStream EditorEvent -> EvStream CursorEvent
    moveEvs = filterMapEs $ \case
      EditorInsert _    -> Just MoveInsert
      EditorBackspace   -> Just MoveBackspace
      EditorMoveDir dir -> Just (Move dir)
      EditorMoveAbs pos -> Just (MoveAbs pos)
      _ -> Nothing
