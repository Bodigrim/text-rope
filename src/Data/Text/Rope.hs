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
import Data.Ord (Ord, compare, (<), (<=))
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text.Lines (Position(..))
import qualified Data.Text.Lines as TL
import qualified Data.Text.Lines.Internal as TL (newlines)
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
-- or "Data.Text.Mixed.Rope", if you need both interfaces.
data Rope
  = Empty
  | Node
    { _ropeLeft    :: !Rope
    , _ropeMiddle  :: !TL.TextLines
    , _ropeRight   :: !Rope
    , _ropeMetrics :: {-# UNPACK #-} !Metrics
    }

data Metrics = Metrics
  { _metricsNewlines :: !Word
  , _metricsCharLen  :: !Word
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
  Metrics nls1 c1 <> Metrics nls2 c2 =
    Metrics (nls1 + nls2) (c1 + c2)
  {-# INLINE (<>) #-}

instance Monoid Metrics where
  mempty = Metrics 0 0
  mappend = (<>)

subMetrics :: Metrics -> Metrics -> Metrics
subMetrics (Metrics nls1 c1) (Metrics nls2 c2) =
  Metrics (nls1 - nls2) (c1 - c2)

metrics :: Rope -> Metrics
metrics = \case
  Empty -> mempty
  Node _ _ _ m -> m

linesMetrics :: TL.TextLines -> Metrics
linesMetrics tl = Metrics
  { _metricsNewlines = TL.newlines tl
  , _metricsCharLen = TL.length tl
  }

#ifdef DEBUG
deriving instance Show Metrics
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

-- | The number of newline characters, O(1).
--
-- >>> :set -XOverloadedStrings
-- >>> newlines ""
-- 0
-- >>> newlines "foo"
-- 0
-- >>> newlines "foo\n"
-- 1
-- >>> newlines "foo\n\n"
-- 2
-- >>> newlines "foo\nbar"
-- 1
--
newlines :: Rope -> Word
newlines = _metricsNewlines . metrics

-- | Measure text length as an amount of lines and columns.
-- Time is linear in the length of the last line.
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
lengthAsPosition rp =
  Position nls (length line)
  where
    nls = newlines rp
    (_, line) = splitAtLine nls rp

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

-- | Create a 'Node', defragmenting it if necessary. The 'Metrics' argument is
-- the computed metrics of the 'TL.TextLines' argument.
node :: HasCallStack => Rope -> TL.TextLines -> Metrics -> Rope -> Rope
node l c cm r = defragment l c r (metrics l <> cm <> metrics r)

-- | Append a 'TL.TextLines' with the given 'Metrics' to a 'Rope'.
snoc :: Rope -> TL.TextLines -> Metrics -> Rope
snoc tr tl tlm
  | TL.null tl = tr
  | otherwise = node tr tl tlm Empty

-- | Prepend a 'TL.TextLines' with the given 'Metrics' to a 'Rope'.
cons :: TL.TextLines -> Metrics -> Rope -> Rope
cons tl tlm tr
  | TL.null tl = tr
  | otherwise = node Empty tl tlm tr

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
  Node l c r m
    | len <= ll -> case splitAt len l of
        (before, after) -> (before, node after c cm r)
    | len <= llc -> do
      let i = len - ll
      case TL.splitAt i c of
        (before, after) -> do
          let beforeMetrics = Metrics
                { _metricsNewlines = TL.newlines before
                , _metricsCharLen = i
                }
          let afterMetrics = subMetrics cm beforeMetrics
          (snoc l before beforeMetrics, cons after afterMetrics r)
    | otherwise -> case splitAt (len - llc) r of
      (before, after) -> (node l c cm before, after)
    where
      ll = length l
      llc = ll + _metricsCharLen cm
      cm = subMetrics m (metrics l <> metrics r)

-- | Split at given line, logarithmic time.
--
-- >>> :set -XOverloadedStrings
-- >>> map (\l -> splitAtLine l "foo\nbar") [0..3]
-- [("","foo\nbar"),("foo\n","bar"),("foo\nbar",""),("foo\nbar","")]
--
splitAtLine :: HasCallStack => Word -> Rope -> (Rope, Rope)
splitAtLine !len = \case
  Empty -> (Empty, Empty)
  Node l c r m
    | len <= ll -> case splitAtLine len l of
      (before, after) -> (before, node after c cm r)
    | len <= llc -> case TL.splitAtLine (len - ll) c of
      (before, after) -> (snoc l before (linesMetrics before), cons after (linesMetrics after) r)
    | otherwise -> case splitAtLine (len - llc) r of
      (before, after) -> (node l c cm before, after)
    where
      ll = newlines l
      llc = ll + _metricsNewlines cm
      cm = subMetrics m (metrics l <> metrics r)

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
splitAtPosition (Position l c) rp = (beforeLine <> beforeColumn, afterColumn)
  where
    (beforeLine, afterLine) = splitAtLine l rp
    (beforeColumn, afterColumn) = splitAt c afterLine
