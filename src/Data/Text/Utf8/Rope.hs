-- |
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- @since 0.3

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

module Data.Text.Utf8.Rope
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
  , getLine
  -- * UTF-8 code units
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
import Data.Text.Utf8.Lines (Position(..))
import qualified Data.Text.Utf8.Lines as TL
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
-- This rope offers an interface, based on UTF-8 code units.
-- Use "Data.Text.Rope", if you need code points,
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
  , _metricsUtf8Len  :: !Word
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
  Metrics nls1 u1 <> Metrics nls2 u2 =
    Metrics (nls1 + nls2) (u1 + u2)
  {-# INLINE (<>) #-}

instance Monoid Metrics where
  mempty = Metrics 0 0
  mappend = (<>)

subMetrics :: Metrics -> Metrics -> Metrics
subMetrics (Metrics nls1 u1) (Metrics nls2 u2) =
  Metrics (nls1 - nls2) (u1 - u2)

metrics :: Rope -> Metrics
metrics = \case
  Empty -> mempty
  Node _ _ _ m -> m

linesMetrics :: TL.TextLines -> Metrics
linesMetrics tl = Metrics
  { _metricsNewlines = TL.newlines tl
  , _metricsUtf8Len = TL.length tl
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

-- | Length in UTF-8 code units aka bytes, O(1).
--
-- >>> :set -XOverloadedStrings
-- >>> length "fя𐀀"
-- 7
-- >>> Data.Text.Rope.length "fя𐀀"
-- 3
length :: Rope -> Word
length = _metricsUtf8Len . metrics

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
-- Position {posLine = 0, posColumn = 5}
-- >>> lengthAsPosition "f\n𐀀"
-- Position {posLine = 1, posColumn = 4}
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
  | _metricsUtf8Len m < DEFRAGMENTATION_THRESHOLD
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

-- | Split at given UTF-8 code unit aka byte.
-- If requested number of code units splits a code point in half, return 'Nothing'.
-- Takes linear time.
--
-- >>> :set -XOverloadedStrings
-- >>> map (\c -> splitAt c "fя𐀀") [0..7]
-- [Just ("","fя𐀀"),Just ("f","я𐀀"),Nothing,Just ("fя","𐀀"),Nothing,Nothing,Nothing,Just ("fя𐀀","")]
--
splitAt :: HasCallStack => Word -> Rope -> Maybe (Rope, Rope)
splitAt !len = \case
  Empty -> Just (Empty, Empty)
  Node l c r m
    | len <= ll -> case splitAt len l of
        Nothing -> Nothing
        Just (before, after) -> Just (before, node after c cm r)
    | len <= llc -> do
      let i = len - ll
      case TL.splitAt i c of
        Nothing -> Nothing
        Just (before, after) -> do
          let beforeMetrics = Metrics
                { _metricsNewlines = TL.newlines before
                , _metricsUtf8Len = i
                }
          let afterMetrics = subMetrics cm beforeMetrics
          Just (snoc l before beforeMetrics, cons after afterMetrics r)
    | otherwise -> case splitAt (len - llc) r of
      Nothing -> Nothing
      Just (before, after) -> Just (node l c cm before, after)
    where
      ll = length l
      llc = ll + _metricsUtf8Len cm
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
-- Just ("f\n","𐀀я")
-- >>> splitAtPosition (Position 1 1) "f\n𐀀я"
-- Nothing
-- >>> splitAtPosition (Position 1 4) "f\n𐀀я"
-- Just ("f\n𐀀","я")
-- >>> splitAtPosition (Position 0 2) "f\n𐀀я"
-- Just ("f\n","𐀀я")
-- >>> splitAtPosition (Position 0 3) "f\n𐀀я"
-- Nothing
-- >>> splitAtPosition (Position 0 6) "f\n𐀀я"
-- Just ("f\n𐀀","я")
--
splitAtPosition :: HasCallStack => Position -> Rope -> Maybe (Rope, Rope)
splitAtPosition (Position l c) rp = do
  let (beforeLine, afterLine) = splitAtLine l rp
  (beforeColumn, afterColumn) <- splitAt c afterLine
  Just (beforeLine <> beforeColumn, afterColumn)

-- | Get a line by its 0-based index.
-- Returns @""@ if the index is out of bounds.
-- The result doesn't contain @\\n@ characters.
--
-- >>> :set -XOverloadedStrings
-- >>> map (\l -> getLine l "foo\nbar\n😊😊\n\n") [0..3]
-- ["foo","bar","😊😊",""]
--
-- @since 0.3
getLine :: Word -> Rope -> Text
getLine lineIdx rp =
  case T.unsnoc firstLine of
    Just (firstLineInit, '\n') -> firstLineInit
    _ -> firstLine
  where
    (_, afterIndex) = splitAtLine lineIdx rp
    (firstLineRope, _ ) = splitAtLine 1 afterIndex
    firstLine = toText firstLineRope
