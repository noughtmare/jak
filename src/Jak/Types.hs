{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jak.Types where

import Control.FRPNow
import System.Exit
import qualified Data.Sequence as S
import Control.Lens

--------------------------------------------------------------------------------
-- IO types
--------------------------------------------------------------------------------

newtype Frontend event model = Frontend
  { runNow :: (EvStream event -> Now (model, EvStream model, Event ExitCode)) -> IO () }

emptyFrontend :: Frontend e m
emptyFrontend = Frontend $ \f -> runNowMaster ((\(_,_,x) -> x) <$> f mempty) >>= exitWith

newtype Handler event model = Handler
  { handle :: EvStream event -> model -> Now (model, EvStream model, Event ExitCode) }

emptyHandler :: Handler e m
emptyHandler = Handler $ \_ m -> pure (m, mempty, never)

data IOFrontend resource event model = IOFrontend
  { acquire  :: IO resource
  , release  :: resource -> IO ()
  , getEvent :: resource -> IO event
  , draw     :: resource -> model -> IO ()
  }

--------------------------------------------------------------------------------
-- Internal types
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

-- | This is a type that holds values that are used when moving up or down to
-- memorize the column offset.
newtype VirtualColumn = V Int
  deriving (Eq,Ord,Show,Read,Enum,Num,Real,Bounded,Integral)

v2c :: VirtualColumn -> Column
v2c = fromIntegral

c2v :: Column -> VirtualColumn
c2v = fromIntegral

-- A more efficient* S.Seq (S.Seq ())
--
-- * Benchmark needed
newtype Shape = Shape (S.Seq Int)

data Viewport = Viewport
  { _viewportPosition :: !Position
  , _viewportSize     :: !Size
  }
viewportPosition :: Lens' Viewport Position
viewportPosition = lens _viewportPosition (\x y -> x { _viewportPosition = y })
viewportSize :: Lens' Viewport Size
viewportSize     = lens _viewportSize     (\x y -> x { _viewportSize     = y })

emptyViewport :: Size -> Viewport
emptyViewport = Viewport (Position 0 0)

data Cursor = Cursor
  { _cursorPos :: !Position
  , _cursorVirtCol :: !VirtualColumn
  } deriving Eq
cursorPos :: Lens' Cursor Position
cursorPos     = lens _cursorPos     (\x y -> x { _cursorPos     = y })
cursorVirtCol :: Lens' Cursor VirtualColumn
cursorVirtCol = lens _cursorVirtCol (\x y -> x { _cursorVirtCol = y })

emptyCursor :: Cursor
emptyCursor = Cursor (Position 0 0) 0

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
emptyContent = Content (S.singleton S.empty)

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
