-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

#ifdef DEBUG
#define DEFRAGMENTATION_THRESHOLD 4
#else
#define DEFRAGMENTATION_THRESHOLD 4096
#endif

module Data.Text.Rope
  ( Rope
  , fromText
  , fromTextLines
  , toText
  , toTextLines
  , null
  -- * Lines
  , lines
  , lengthInLines
  , splitAtLine
  -- * Code points
  , length
  , splitAt
  , Position(..)
  , lengthAsPosition
  , splitAtPosition
  ) where

import Prelude ((-), (+), seq)
import Control.DeepSeq (NFData, rnf)
import Data.Bool (Bool(..), otherwise)
import Data.Char (Char)
import Data.Eq (Eq, (==))
import Data.Function ((.), ($), on)
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import Data.Ord (Ord, compare, (<), (<=), Ordering(..))
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text.Lines (Position(..))
import qualified Data.Text.Lines as TL
import Data.Word (Word)
import Text.Show (Show)

#ifdef DEBUG
import Prelude (error)
import GHC.Stack (HasCallStack)
#else
#define HasCallStack ()
import Text.Show (show)
#endif

-- | Rope of 'Text' chunks with logarithmic concatenation.
-- This rope offers an interface, based on code points.
-- Use "Data.Text.Utf16.Rope", if you need UTF-16 code units,
-- or "Data.Text.Utf16.Rope.Mixed", if you need both interfaces.
data Rope
  = Empty
  | Node
    { _ropeLeft    :: !Rope
    , _ropeMiddle  :: !TL.TextLines
    , _ropeRight   :: !Rope
    , _ropeMetrics :: {-# UNPACK #-} !Metrics
    }

data Metrics = Metrics
  { _metricsCharLen      :: !Word
  , _metricsCharLenAsPos :: !Position
  }

instance NFData Rope where
  rnf Empty = ()
  -- No need to deepseq strict fields, for which WHNF = NF
  rnf (Node l _ r _) = rnf l `seq` rnf r

instance Eq Rope where
  (==) = (==) `on` toLazyText

instance Ord Rope where
  compare = compare `on` toLazyText

instance Semigroup Metrics where
  Metrics c1 p1 <> Metrics c2 p2 =
    Metrics (c1 + c2) (p1 <> p2)

instance Monoid Metrics where
  mempty = Metrics 0 mempty

metrics :: Rope -> Metrics
metrics = \case
  Empty -> mempty
  Node _ _ _ m -> m

linesMetrics :: TL.TextLines -> Metrics
linesMetrics tl = Metrics
  { _metricsCharLen = TL.length tl
  , _metricsCharLenAsPos = TL.lengthAsPosition tl
  }

#ifdef DEBUG
deriving instance Show Rope
#else
instance Show Rope where
  show = show . toLazyText
#endif

instance IsString Rope where
  fromString = fromTextLines . fromString

-- | Check whether a rope is empty, O(1).
null :: Rope -> Bool
null = \case
  Empty -> True
  Node{} -> False

-- | Length in code points, similar to @Data.Text.@'Data.Text.length', O(1).
--
-- >>> :set -XOverloadedStrings
-- >>> length "fя𐀀"
-- 3
-- >>> Data.Text.Utf16.Rope.length "fя𐀀"
-- 4
--
length :: Rope -> Word
length = _metricsCharLen . metrics

-- | Measure text length as an amount of lines and columns, O(1).
--
-- >>> :set -XOverloadedStrings
-- >>> lengthAsPosition "f𐀀"
-- Position {posLine = 0, posColumn = 2}
-- >>> lengthAsPosition "f\n𐀀"
-- Position {posLine = 1, posColumn = 1}
-- >>> lengthAsPosition "f\n𐀀\n"
-- Position {posLine = 2, posColumn = 0}
--
lengthAsPosition :: Rope -> Position
lengthAsPosition = _metricsCharLenAsPos . metrics

instance Semigroup Rope where
  Empty <> t = t
  t <> Empty = t
  Node l1 c1 r1 m1 <> Node l2 c2 r2 m2 = defragment
    l1
    c1
    (Node (r1 <> l2) c2 r2 (metrics r1 <> m2))
    (m1 <> m2)

instance Monoid Rope where
  mempty = Empty
  mappend = (<>)

defragment :: HasCallStack => Rope -> TL.TextLines -> Rope -> Metrics -> Rope
defragment !l !c !r !m
#ifdef DEBUG
  | TL.null c = error "Data.Text.Lines: violated internal invariant"
#endif
  | _metricsCharLen m < DEFRAGMENTATION_THRESHOLD
  = Node Empty (toTextLines rp) Empty m
  | otherwise
  = rp
  where
    rp = Node l c r m

-- | Create from 'TL.TextLines', linear time.
fromTextLines :: TL.TextLines -> Rope
fromTextLines tl
  | TL.null tl = Empty
  | otherwise = Node Empty tl Empty (linesMetrics tl)

node :: HasCallStack => Rope -> TL.TextLines -> Rope -> Rope
node l c r = defragment l c r (metrics l <> linesMetrics c <> metrics r)

(|>) :: Rope -> TL.TextLines -> Rope
tr |> tl
  | TL.null tl = tr
  | otherwise = node tr tl Empty

(<|) :: TL.TextLines -> Rope -> Rope
tl <| tr
  | TL.null tl = tr
  | otherwise = node Empty tl tr

-- | Create from 'Text', linear time.
fromText :: Text -> Rope
fromText = fromTextLines . TL.fromText

foldMapRope :: Monoid a => (TL.TextLines -> a) -> Rope -> a
foldMapRope f = go
  where
    go = \case
      Empty -> mempty
      Node l c r _ -> go l `mappend` f c `mappend` go r

data Lines = Lines ![Text] !Bool

instance Semigroup Lines where
  Lines [] _ <> ls = ls
  ls <> Lines [] _ = ls
  Lines xs x <> Lines ys y = Lines (if x then xs <> ys else go xs ys) y
    where
      go [] vs = vs
      go [u] (v : vs) = (u <> v) : vs
      go (u : us) vs = u : go us vs

instance Monoid Lines where
  mempty = Lines [] False
  mappend = (<>)

-- | Split into lines by @\\n@, similar to @Data.Text.@'Data.Text.lines'.
-- Each line is produced in O(1).
--
-- >>> :set -XOverloadedStrings
-- >>> lines ""
-- []
-- >>> lines "foo"
-- ["foo"]
-- >>> lines "foo\n"
-- ["foo"]
-- >>> lines "foo\n\n"
-- ["foo",""]
-- >>> lines "foo\nbar"
-- ["foo","bar"]
--
lines :: Rope -> [Text]
lines = (\(Lines ls _) -> ls) . foldMapRope
  -- This assumes that there are no empty chunks:
  (\tl -> Lines (TL.lines tl) (T.last (TL.toText tl) == '\n'))

lastChar :: Rope -> Maybe Char
lastChar = \case
  Empty -> Nothing
  -- This assumes that there are no empty chunks:
  Node _ c Empty _ -> Just $ T.last $ TL.toText c
  Node _ _ r _ -> lastChar r

-- | Equivalent to 'Data.List.length' . 'lines', but in logarithmic time.
--
-- >>> :set -XOverloadedStrings
-- >>> lengthInLines ""
-- 0
-- >>> lengthInLines "foo"
-- 1
-- >>> lengthInLines "foo\n"
-- 1
-- >>> lengthInLines "foo\n\n"
-- 2
-- >>> lengthInLines "foo\nbar"
-- 2
--
-- If you do not care about ignoring the last newline character,
-- you can use 'posLine' . 'lengthAsPosition' instead, which works in O(1).
--
lengthInLines :: Rope -> Word
lengthInLines rp = case lastChar rp of
  Nothing -> 0
  Just ch -> TL.posLine (lengthAsPosition rp) + (if ch == '\n' then 0 else 1)

-- | Glue chunks into 'TL.TextLines', linear time.
toTextLines :: Rope -> TL.TextLines
toTextLines = mconcat . foldMapRope (:[])

toLazyText :: Rope -> TextLazy.Text
toLazyText = foldMapRope (TextLazy.fromStrict . TL.toText)

-- | Glue chunks into 'Text', linear time.
toText :: Rope -> Text
toText = TextLazy.toStrict . Builder.toLazyText . foldMapRope (Builder.fromText . TL.toText)

-- | Split at given code point, similar to @Data.Text.@'Data.Text.splitAt'.
-- Takes linear time.
--
-- >>> :set -XOverloadedStrings
-- >>> map (\c -> splitAt c "fя𐀀") [0..4]
-- [("","fя𐀀"),("f","я𐀀"),("fя","𐀀"),("fя𐀀",""),("fя𐀀","")]
--
splitAt :: HasCallStack => Word -> Rope -> (Rope, Rope)
splitAt !len = \case
  Empty -> (Empty, Empty)
  Node l c r _
    | len <= ll -> case splitAt len l of
        (before, after) -> (before, node after c r)
    | len <= llc -> case TL.splitAt (len - ll) c of
      (before, after) -> (l |> before, after <| r)
    | otherwise -> case splitAt (len - llc) r of
      (before, after) -> (node l c before, after)
    where
      ll = length l
      llc = ll + TL.length c

-- | Split at given line, logarithmic time.
--
-- >>> :set -XOverloadedStrings
-- >>> map (\l -> splitAtLine l "foo\nbar") [0..3]
-- [("","foo\nbar"),("foo\n","bar"),("foo\nbar",""),("foo\nbar","")]
--
splitAtLine :: HasCallStack => Word -> Rope -> (Rope, Rope)
splitAtLine !len = \case
  Empty -> (Empty, Empty)
  Node l c r _
    | len <= ll -> case splitAtLine len l of
      (before, after) -> (before, node after c r)
    | len <= llc -> case TL.splitAtLine (len - ll) c of
      (before, after) -> (l |> before, after <| r)
    | otherwise -> case splitAtLine (len - llc) r of
      (before, after) -> (node l c before, after)
    where
      ll = TL.posLine (lengthAsPosition l)
      llc = ll + TL.posLine (TL.lengthAsPosition c)

subOnRope :: Rope -> Position -> Position -> Position
subOnRope rp (Position xl xc) (Position yl yc) = case xl `compare` yl of
  GT -> Position (xl - yl) xc
  EQ -> Position 0 (xc - yc)
  LT -> Position 0 (xc - length rp')
  where
    (_, rp') = splitAtLine xl rp

subOnLines :: TL.TextLines -> Position -> Position -> Position
subOnLines tl (Position xl xc) (Position yl yc) = case xl `compare` yl of
  GT -> Position (xl - yl) xc
  EQ -> Position 0 (xc - yc)
  LT -> Position 0 (xc - TL.length tl')
  where
    (_, tl') = TL.splitAtLine xl tl

-- | Combination of 'splitAtLine' and subsequent 'splitAt'.
-- Time is linear in 'posColumn' and logarithmic in 'posLine'.
--
-- >>> :set -XOverloadedStrings
-- >>> splitAtPosition (Position 1 0) "f\n𐀀я"
-- ("f\n","𐀀я")
-- >>> splitAtPosition (Position 1 1) "f\n𐀀я"
-- ("f\n𐀀","я")
-- >>> splitAtPosition (Position 1 2) "f\n𐀀я"
-- ("f\n𐀀я","")
-- >>> splitAtPosition (Position 0 2) "f\n𐀀я"
-- ("f\n","𐀀я")
-- >>> splitAtPosition (Position 0 3) "f\n𐀀я"
-- ("f\n𐀀","я")
-- >>> splitAtPosition (Position 0 4) "f\n𐀀я"
-- ("f\n𐀀я","")
--
splitAtPosition :: HasCallStack => Position -> Rope -> (Rope, Rope)
splitAtPosition (Position 0 0) = (mempty,)
splitAtPosition !len = \case
  Empty -> (Empty, Empty)
  Node l c r _
    | len <= ll -> case splitAtPosition len l of
      (before, after)
        | null after -> case splitAtPosition len' (c <| r) of
          (r', r'') -> (l <> r', r'')
        | otherwise -> (before, node after c r)
    | len <= llc -> case TL.splitAtPosition len' c of
      (before, after)
        | TL.null after -> case splitAtPosition len'' r of
          (r', r'') -> ((l |> c) <> r', r'')
        | otherwise -> (l |> before, after <| r)
    | otherwise -> case splitAtPosition len'' r of
      (before, after) -> (node l c before, after)
    where
      ll = lengthAsPosition l
      lc = TL.lengthAsPosition c
      llc = ll <> lc
      len' = subOnRope l len ll
      len'' = subOnLines c len' lc
