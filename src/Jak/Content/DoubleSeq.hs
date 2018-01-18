{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
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
-- The main alternatives for me would be Yampa, but I didn't want to learn how to
-- use the arrow interface. Or maybe reflex when I'm more confortable with FRP.
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

import Control.Monad.State
import Control.Lens
import Control.Applicative
import Control.FRPNow
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as S
import qualified Jak.Core as J

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Size     = Size     !Width  !Height
  deriving Show
data Position = Position !Column !Row
  deriving Eq

instance Ord Position where
  compare (Position c r) (Position c' r') = case compare r r' of
    LT -> LT
    GT -> GT
    EQ -> compare c c'

s2p :: Size -> Position
s2p (Size w h) = Position (w2c w) (h2r h)

p2s :: Position -> Size
p2s (Position c r) = Size (c2w c) (r2h r)

-- Type safety!

-- NOTE: Linear.Affine from Edward Kmett's linear library could help with
-- type safety and usability, but I am too lazy to implement it now.
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

--------------------------------------------------------------------------------
-- Viewport
--------------------------------------------------------------------------------

data Viewport = Viewport
  { _viewportPosition :: !Position
  , _viewportSize :: !Size
  }

makeLenses ''Viewport

data ViewportEvent
  = Resize !Size
  | Moved !Position

emptyViewport :: Size -> Viewport
emptyViewport = Viewport (Position 0 0)

scrollViewport :: ViewportEvent -> Viewport -> Viewport
scrollViewport (Moved (Position cc cr)) (Viewport (Position vc vr) size@(Size w h))
  = let c | cc < vc          = cc
          | cc >= vc + w2c w = cc - w2c w + 1
          | otherwise        = vc
        r | cr < vr          = cr
          | cr >= vr + h2r h = cr - h2r h + 1
          | otherwise        = vr
    in  Viewport (Position c r) size
scrollViewport (Resize size) (Viewport pos _) = Viewport pos size

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

-- This is a type that holds values that are used when moving up or down to
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
  }

makeLenses ''Cursor

data CursorEvent
  = MoveNorth
  | MoveEast Bool
  | MoveSouth
  | MoveWest Bool
  | MoveAbs Column Row

newtype Shape = Shape (S.Seq Int)

emptyCursor :: Cursor
emptyCursor = Cursor (Position 0 0) 0

moveCursor :: (Shape,CursorEvent) -> Cursor -> Cursor
moveCursor (Shape shape, move) (Cursor (Position c r) v) =
  let (Position c' r') = newPos
      r'' = min (R (length shape)) r'
      c'' = min (C (S.index shape (fromIntegral r''))) c'
  in Cursor (Position c'' r'') (c2v c')
  where
    lineLength :: Column
    lineLength = C $ S.index shape (fromIntegral r)
    contentLength :: Row
    contentLength = R $ length shape
    newPos = case move of
      MoveNorth     -> Position (v2c v) (max 0 (r - 1))
      MoveEast stayOnLine | stayOnLine || c < lineLength -> Position (min lineLength (c + 1)) r
                          | r < contentLength -> Position 0 (r + 1)
                          | otherwise -> Position c r
      MoveSouth     -> Position (v2c v) (min (contentLength - 1) (r + 1))
      MoveWest stayOnLine | stayOnLine || c > 0 -> Position (max 0 (c - 1)) r
                          | r > 0               -> Position (C (S.index shape (fromIntegral (r - 1)))) (r - 1)
                          | otherwise           -> Position c r
      MoveAbs c' r' -> Position c' r'

--------------------------------------------------------------------------------
-- Content
--------------------------------------------------------------------------------

newtype Content = Content
  { _contentSeq :: (S.Seq (S.Seq Char))
  }

makeLenses ''Content

data ContentEvent = Replace !Range !String
data Range = Range !Position !Size

emptyContent :: Content
emptyContent = Content S.empty

replaceContent :: ContentEvent -> Content -> Content
replaceContent (Replace (Range pos size) as) (Content s) =
   let (a,b') = splitAtPosition pos s
       b = dropUntilPosition (s2p size) b'
   in Content (overlap3 a (toDoubleSeq as) b)

dropUntilPosition :: Position -> S.Seq (S.Seq Char) -> S.Seq (S.Seq Char)
dropUntilPosition (Position (C c) (R r)) = f . S.drop r
  where
    f S.Empty = S.Empty
    f (x S.:<| xs) = S.drop c x S.:<| xs

splitAtPosition :: Position -> S.Seq (S.Seq Char) -> (S.Seq (S.Seq Char),S.Seq (S.Seq Char))
splitAtPosition (Position (C col) (R row)) s = case S.splitAt row s of
  (a,b S.:<| c) -> let (d,e) = S.splitAt col b in (a S.:|> d, e S.:<| c)
  (a,S.Empty) -> (a,S.Empty)

overlap3 :: S.Seq (S.Seq Char) -> S.Seq (S.Seq Char) -> S.Seq (S.Seq Char) -> S.Seq (S.Seq Char)
overlap3 a b c = a `overlap2` b `overlap2` c

overlap2 :: S.Seq (S.Seq Char) -> S.Seq (S.Seq Char) -> S.Seq (S.Seq Char)
overlap2 S.Empty      b            = b
overlap2 a            S.Empty      = a
overlap2 (a S.:|> aa) (bb S.:<| b) = mconcat [a, S.singleton (aa <> bb), b]

toDoubleSeq :: String -> S.Seq (S.Seq Char)
toDoubleSeq = S.fromList . map S.fromList . lines'
  where
    -- Proper lines implementation ;)
    lines' :: String -> [String]
    lines' = foldr (\a (b:bs) -> if a == '\n' then []:b:bs else (a:b):bs) [[]]

--------------------------------------------------------------------------------
-- Editor
--------------------------------------------------------------------------------

data Editor = Editor
  { _editorViewport :: !Viewport
  , _editorCursor   :: !Cursor
  , _editorContent  :: !Content
  }

makeLenses ''Editor

data EditorEvent
  = EditorInsert String
  | EditorBackspace
  | EditorDelete
  | EditorMoveLeft
  | EditorMoveRight
  | EditorMoveUp
  | EditorMoveDown
  | EditorMove Position
  | EditorResize Size

emptyEditor :: Size -> Editor
emptyEditor size = Editor (emptyViewport size) emptyCursor emptyContent

editor :: Editor -> EvStream EditorEvent -> Now (EvStream Editor)
editor editor evs = sample (scanlEv (\a b -> execState (editorEventS b) a) editor evs)

-- | Move the cursor using shape information from the content
moveCursorWithShape :: CursorEvent -> State Editor ()
moveCursorWithShape evt = do
  shape <- Shape . fmap length <$> use (editorContent . contentSeq)
  editorCursor %= moveCursor (shape,evt)

-- | Update the viewport after the cursor has moved
updateViewport :: State Editor ()
updateViewport = do
  c <- use (editorCursor . cursorPos)
  editorViewport %= scrollViewport (Moved c)

editorEventS :: EditorEvent -> State Editor ()
editorEventS = \case
  EditorMoveLeft  -> moveCursorWithShape (MoveWest True) *> updateViewport
  EditorMoveRight -> moveCursorWithShape (MoveEast True) *> updateViewport
  EditorMoveUp    -> moveCursorWithShape MoveNorth       *> updateViewport
  EditorMoveDown  -> moveCursorWithShape MoveSouth       *> updateViewport
  EditorResize newSize -> editorViewport . viewportSize .= newSize
  EditorInsert ""      -> pure ()
  EditorInsert str@[_] -> do
    c <- use (editorCursor . cursorPos)
    editorContent %= replaceContent (Replace (Range c (Size 0 0)) str)
    moveCursorWithShape (MoveEast False)
    updateViewport
  EditorInsert _ -> error "Can't insert multiple character yet"
  EditorBackspace -> do
    c <- use (editorCursor . cursorPos)
    moveCursorWithShape (MoveWest False)
    c' <- use (editorCursor . cursorPos)
    editorContent %= replaceContent (Replace (rangeFromTo c c') "")
    updateViewport
  EditorDelete -> do
    c <- use (editorCursor . cursorPos)
    editorContent %= replaceContent (Replace (Range c (Size 1 0)) "")

rangeFromTo :: Position -> Position -> Range
rangeFromTo p@(Position c r) q@(Position c' r')
  = Range (min p q) (Size (c2w (abs (c' - c))) (r2h (abs (r' - r))))
