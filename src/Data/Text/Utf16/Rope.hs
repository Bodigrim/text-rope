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

module Data.Text.Utf16.Rope
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
  -- * UTF-16 code units
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
import Data.Text.Utf16.Lines (Position(..))
import qualified Data.Text.Utf16.Lines as TL
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
-- This rope offers an interface, based on UTF-16 code units.
-- Use "Data.Text.Rope", if you need code points,
-- or "Data.Text.Utf16.Rope.Mixed", if you need both interfaces.
data Rope
  = Empty
  | Node
    { _ropeLeft          :: !Rope
    , _ropeMiddle        :: !TL.TextLines
    , _ropeRight         :: !Rope
    , _ropeUtf16Len      :: !Word
    , _ropeUtf16LenAsPos :: !Position
    }

instance NFData Rope where
  rnf Empty = ()
  -- No need to deepseq strict fields, for which WHNF = NF
  rnf (Node l _ r _ _) = rnf l `seq` rnf r

instance Eq Rope where
  (==) = (==) `on` toLazyText

instance Ord Rope where
  compare = compare `on` toLazyText

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

-- | Length in UTF-16 code units, O(1).
--
-- >>> :set -XOverloadedStrings
-- >>> length "fя𐀀"
-- 4
-- >>> Data.Text.Rope.length "fя𐀀"
-- 3
length :: Rope -> Word
length = \case
  Empty -> 0
  Node _ _ _ w _ -> w

-- | Measure text length as an amount of lines and columns, O(1).
--
-- >>> :set -XOverloadedStrings
-- >>> lengthAsPosition "f𐀀"
-- Position {posLine = 0, posColumn = 3}
-- >>> lengthAsPosition "f\n𐀀"
-- Position {posLine = 1, posColumn = 2}
-- >>> lengthAsPosition "f\n𐀀\n"
-- Position {posLine = 2, posColumn = 0}
--
lengthAsPosition :: Rope -> Position
lengthAsPosition = \case
  Empty -> mempty
  Node _ _ _ _ p -> p

instance Semigroup Rope where
  Empty <> t = t
  t <> Empty = t
  Node l1 c1 r1 u1 p1 <> Node l2 c2 r2 u2 p2 = defragment
    l1
    c1
    (Node (r1 <> l2) c2 r2 (length r1 + u2) (lengthAsPosition r1 <> p2))
    (u1 + u2)
    (p1 <> p2)

instance Monoid Rope where
  mempty = Empty
  mappend = (<>)

defragment :: HasCallStack => Rope -> TL.TextLines -> Rope -> Word -> Position -> Rope
defragment !l !c !r !u !p
#ifdef DEBUG
  | TL.null c = error "Data.Text.Lines: violated internal invariant"
#endif
  | u < DEFRAGMENTATION_THRESHOLD
  = Node Empty (toTextLines rp) Empty u p
  | otherwise
  = rp
  where
    rp = Node l c r u p

-- | Create from 'TL.TextLines', linear time.
fromTextLines :: TL.TextLines -> Rope
fromTextLines tl
  | TL.null tl = Empty
  | otherwise = Node Empty tl Empty (TL.length tl) (TL.lengthAsPosition tl)

node :: HasCallStack => Rope -> TL.TextLines -> Rope -> Rope
node l c r = defragment l c r totalLength totalLengthAsPosition
  where
    totalLength = length l + TL.length c + length r
    totalLengthAsPosition = lengthAsPosition l <> TL.lengthAsPosition c <> lengthAsPosition r

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
      Node l c r _ _ -> go l `mappend` f c `mappend` go r

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
  Node _ c Empty _ _ -> Just $ T.last $ TL.toText c
  Node _ _ r _ _ -> lastChar r

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

-- | Split at given UTF-16 code unit.
-- If requested number of code units splits a code point in half, return 'Nothing'.
-- Takes linear time.
--
-- >>> :set -XOverloadedStrings
-- >>> map (\c -> splitAt c "fя𐀀") [0..4]
-- [Just ("","fя𐀀"),Just ("f","я𐀀"),Just ("fя","𐀀"),Nothing,Just ("fя𐀀","")]
--
splitAt :: HasCallStack => Word -> Rope -> Maybe (Rope, Rope)
splitAt !len = \case
  Empty -> Just (Empty, Empty)
  Node l c r _ _
    | len <= ll -> case splitAt len l of
        Nothing -> Nothing
        Just (before, after) -> Just (before, node after c r)
    | len <= llc -> case TL.splitAt (len - ll) c of
      Nothing -> Nothing
      Just (before, after) -> Just (l |> before, after <| r)
    | otherwise -> case splitAt (len - llc) r of
      Nothing -> Nothing
      Just (before, after) -> Just (node l c before, after)
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
  Node l c r _ _
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
-- Just ("f\n","𐀀я")
-- >>> splitAtPosition (Position 1 1) "f\n𐀀я"
-- Nothing
-- >>> splitAtPosition (Position 1 2) "f\n𐀀я"
-- Just ("f\n𐀀","я")
-- >>> splitAtPosition (Position 0 2) "f\n𐀀я"
-- Just ("f\n","𐀀я")
-- >>> splitAtPosition (Position 0 3) "f\n𐀀я"
-- Nothing
-- >>> splitAtPosition (Position 0 4) "f\n𐀀я"
-- Just ("f\n𐀀","я")
--
splitAtPosition :: HasCallStack => Position -> Rope -> Maybe (Rope, Rope)
splitAtPosition (Position 0 0) = Just . (mempty,)
splitAtPosition !len = \case
  Empty -> Just (Empty, Empty)
  Node l c r _ _
    | len <= ll -> case splitAtPosition len l of
      Nothing -> Nothing
      Just (before, after)
        | null after -> case splitAtPosition len' (c <| r) of
          Nothing -> Nothing
          Just (r', r'') -> Just (l <> r', r'')
        | otherwise -> Just (before, node after c r)
    | len <= llc -> case TL.splitAtPosition len' c of
      Nothing -> Nothing
      Just (before, after)
        | TL.null after -> case splitAtPosition len'' r of
          Nothing -> Nothing
          Just (r', r'') -> Just ((l |> c) <> r', r'')
        | otherwise -> Just (l |> before, after <| r)
    | otherwise -> case splitAtPosition len'' r of
      Nothing -> Nothing
      Just (before, after) -> Just (node l c before, after)
    where
      ll = lengthAsPosition l
      lc = TL.lengthAsPosition c
      llc = ll <> lc
      len' = subOnRope l len ll
      len'' = subOnLines c len' lc
