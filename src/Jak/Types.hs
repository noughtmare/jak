{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jak.Types where

import qualified Data.Sequence as S

--------------------------------------------------------------------------------
-- Internal types
--------------------------------------------------------------------------------

-- Strong types!
-- https://www.fpcomplete.com/blog/2018/01/weakly-typed-haskell

-- TODO:
-- Instead of having all these awkward conversion functions
-- there should be functions that operate directly on these
-- strong types.
--
-- It is worth taking a look at Linear.Affine from the linear package.
s2p :: Size -> Position
s2p (Size w h) = Position (w2c w) (h2r h)

p2s :: Position -> Size
p2s (Position c r) = Size (c2w c) (r2h r)

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
-- memorize the column offset. The cursor can be to the left of this column
-- if the line is too short.
newtype VirtualColumn = V Int
  deriving (Eq,Ord,Show,Read,Enum,Num,Real,Bounded,Integral)

v2c :: VirtualColumn -> Column
v2c = fromIntegral

c2v :: Column -> VirtualColumn
c2v = fromIntegral

-- The line lengths of the content
newtype Shape = Shape (S.Seq Int)

class HasShape a where
  projectShape :: a -> Shape

class HasPosition a where
  projectPosition :: a -> Position

class Render a where
  renderCursor :: a -> Position
  renderContent :: a -> [String]

class HasView a where
  projectView :: a -> (Position, Size)

class ViewContent a where
  viewContent :: a -> (Position, Size) -> [String]

class ViewCursor a where
  viewCursor :: a -> (Position, Size) -> Position
