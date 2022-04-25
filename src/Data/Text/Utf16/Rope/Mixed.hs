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

module Data.Text.Utf16.Rope.Mixed
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
  , charLength
  , charSplitAt
  , charLengthAsPosition
  , charSplitAtPosition
  -- * UTF-16 code units
  , utf16Length
  , utf16SplitAt
  , utf16LengthAsPosition
  , utf16SplitAtPosition
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
import Data.Text.Lines.Internal (TextLines)
import qualified Data.Text.Lines.Internal as TL (null, fromText, toText, lines, splitAtLine)
import qualified Data.Text.Lines as Char
import qualified Data.Text.Utf16.Lines as Utf16
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
-- This rope offers two interfaces: one based on code points
-- and another one based on UTF-16 code units. This comes with a price
-- of double bookkeeping and is less performant than "Data.Text.Rope"
-- or "Data.Text.Utf16.Rope".
data Rope
  = Empty
  | Node
    { _ropeLeft          :: !Rope
    , _ropeMiddle        :: !TextLines
    , _ropeRight         :: !Rope
    , _ropeCharLen       :: !Word
    , _ropeCharLenAsPos  :: !Char.Position
    , _ropeUtf16Len      :: !Word
    , _ropeUtf16LenAsPos :: !Utf16.Position
    }

instance NFData Rope where
  rnf Empty = ()
  -- No need to deepseq strict fields, for which WHNF = NF
  rnf (Node l _ r _ _ _ _) = rnf l `seq` rnf r

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

-- | Length in code points, similar to @Data.Text.@'Data.Text.length', O(1).
--
-- >>> :set -XOverloadedStrings
-- >>> charLength "fя𐀀"
-- 3
--
charLength :: Rope -> Word
charLength = \case
  Empty -> 0
  Node _ _ _ w _ _ _ -> w

-- | Length in UTF-16 code units, O(1).
--
-- >>> :set -XOverloadedStrings
-- >>> utf16Length "fя𐀀"
-- 4
--
utf16Length :: Rope -> Word
utf16Length = \case
  Empty -> 0
  Node _ _ _ _ _ w _ -> w

-- | Measure text length as an amount of lines and columns, O(1).
--
-- >>> :set -XOverloadedStrings
-- >>> charLengthAsPosition "f𐀀"
-- Position {posLine = 0, posColumn = 2}
-- >>> charLengthAsPosition "f\n𐀀"
-- Position {posLine = 1, posColumn = 1}
-- >>> charLengthAsPosition "f\n𐀀\n"
-- Position {posLine = 2, posColumn = 0}
--
charLengthAsPosition :: Rope -> Char.Position
charLengthAsPosition = \case
  Empty -> mempty
  Node _ _ _ _ p _ _ -> p

-- | Measure text length as an amount of lines and columns, O(1).
--
-- >>> :set -XOverloadedStrings
-- >>> utf16LengthAsPosition "f𐀀"
-- Position {posLine = 0, posColumn = 3}
-- >>> utf16LengthAsPosition "f\n𐀀"
-- Position {posLine = 1, posColumn = 2}
-- >>> utf16LengthAsPosition "f\n𐀀\n"
-- Position {posLine = 2, posColumn = 0}
--
utf16LengthAsPosition :: Rope -> Utf16.Position
utf16LengthAsPosition = \case
  Empty -> mempty
  Node _ _ _ _ _ _ p -> p

instance Semigroup Rope where
  Empty <> t = t
  t <> Empty = t
  Node l1 c1 r1 u1 p1 u1' p1' <> Node l2 c2 r2 u2 p2 u2' p2' = defragment
    l1
    c1
    (Node (r1 <> l2) c2 r2 (charLength r1 + u2) (charLengthAsPosition r1 <> p2) (utf16Length r1 + u2') (utf16LengthAsPosition r1 <> p2'))
    (u1 + u2)
    (p1 <> p2)
    (u1' + u2')
    (p1' <> p2')

instance Monoid Rope where
  mempty = Empty
  mappend = (<>)

defragment :: HasCallStack => Rope -> TextLines -> Rope -> Word -> Char.Position -> Word -> Utf16.Position -> Rope
defragment !l !c !r !u !p !u' !p'
#ifdef DEBUG
  | TL.null c = error "Data.Text.Lines: violated internal invariant"
#endif
  | u < DEFRAGMENTATION_THRESHOLD
  = Node Empty (toTextLines rp) Empty u p u' p'
  | otherwise
  = rp
  where
    rp = Node l c r u p u' p'

-- | Create from 'TextLines', linear time.
fromTextLines :: TextLines -> Rope
fromTextLines tl
  | TL.null tl = Empty
  | otherwise = Node Empty tl Empty (Char.length tl) (Char.lengthAsPosition tl) (Utf16.length tl) (Utf16.lengthAsPosition tl)

node :: HasCallStack => Rope -> TextLines -> Rope -> Rope
node l c r = defragment l c r totalLength totalLengthAsPosition totalLength' totalLengthAsPosition'
  where
    totalLength = charLength l + Char.length c + charLength r
    totalLengthAsPosition = charLengthAsPosition l <> Char.lengthAsPosition c <> charLengthAsPosition r
    totalLength' = utf16Length l + Utf16.length c + utf16Length r
    totalLengthAsPosition' = utf16LengthAsPosition l <> Utf16.lengthAsPosition c <> utf16LengthAsPosition r

(|>) :: Rope -> TextLines -> Rope
tr |> tl
  | TL.null tl = tr
  | otherwise = node tr tl Empty

(<|) :: TextLines -> Rope -> Rope
tl <| tr
  | TL.null tl = tr
  | otherwise = node Empty tl tr

-- | Create from 'Text', linear time.
fromText :: Text -> Rope
fromText = fromTextLines . TL.fromText

foldMapRope :: Monoid a => (TextLines -> a) -> Rope -> a
foldMapRope f = go
  where
    go = \case
      Empty -> mempty
      Node l c r _ _ _ _ -> go l `mappend` f c `mappend` go r

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
  Node _ c Empty _ _ _ _ -> Just $ T.last $ TL.toText c
  Node _ _ r _ _ _ _ -> lastChar r

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
lengthInLines :: Rope -> Word
lengthInLines rp = case lastChar rp of
  Nothing -> 0
  Just ch -> Char.posLine (charLengthAsPosition rp) + (if ch == '\n' then 0 else 1)

-- | Glue chunks into 'TextLines', linear time.
toTextLines :: Rope -> TextLines
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
-- >>> map (\c -> charSplitAt c "fя𐀀") [0..4]
-- [("","fя𐀀"),("f","я𐀀"),("fя","𐀀"),("fя𐀀",""),("fя𐀀","")]
--
charSplitAt :: HasCallStack => Word -> Rope -> (Rope, Rope)
charSplitAt !len = \case
  Empty -> (Empty, Empty)
  Node l c r _ _ _ _
    | len <= ll -> case charSplitAt len l of
        (before, after) -> (before, node after c r)
    | len <= llc -> case Char.splitAt (len - ll) c of
      (before, after) -> (l |> before, after <| r)
    | otherwise -> case charSplitAt (len - llc) r of
      (before, after) -> (node l c before, after)
    where
      ll = charLength l
      llc = ll + Char.length c

-- | Split at given UTF-16 code unit.
-- If requested number of code units splits a code point in half, return 'Nothing'.
-- Takes linear time.
--
-- >>> :set -XOverloadedStrings
-- >>> map (\c -> utf16SplitAt c "fя𐀀") [0..4]
-- [Just ("","fя𐀀"),Just ("f","я𐀀"),Just ("fя","𐀀"),Nothing,Just ("fя𐀀","")]
--
utf16SplitAt :: HasCallStack => Word -> Rope -> Maybe (Rope, Rope)
utf16SplitAt !len = \case
  Empty -> Just (Empty, Empty)
  Node l c r _ _ _ _
    | len <= ll -> case utf16SplitAt len l of
        Nothing -> Nothing
        Just (before, after) -> Just (before, node after c r)
    | len <= llc -> case Utf16.splitAt (len - ll) c of
      Nothing -> Nothing
      Just (before, after) -> Just (l |> before, after <| r)
    | otherwise -> case utf16SplitAt (len - llc) r of
      Nothing -> Nothing
      Just (before, after) -> Just (node l c before, after)
    where
      ll = utf16Length l
      llc = ll + Utf16.length c

-- | Split at given line, logarithmic time.
--
-- >>> :set -XOverloadedStrings
-- >>> map (\l -> splitAtLine l "foo\nbar") [0..3]
-- [("","foo\nbar"),("foo\n","bar"),("foo\nbar",""),("foo\nbar","")]
--
splitAtLine :: HasCallStack => Word -> Rope -> (Rope, Rope)
splitAtLine !len = \case
  Empty -> (Empty, Empty)
  Node l c r _ _ _ _
    | len <= ll -> case splitAtLine len l of
      (before, after) -> (before, node after c r)
    | len <= llc -> case TL.splitAtLine (len - ll) c of
      (before, after) -> (l |> before, after <| r)
    | otherwise -> case splitAtLine (len - llc) r of
      (before, after) -> (node l c before, after)
    where
      -- posLine is the same both in Char.lengthAsPosition and Utf16.lengthAsPosition
      ll = Char.posLine (charLengthAsPosition l)
      llc = ll + Char.posLine (Char.lengthAsPosition c)

charSubOnRope :: Rope -> Char.Position -> Char.Position -> Char.Position
charSubOnRope rp (Char.Position xl xc) (Char.Position yl yc) = case xl `compare` yl of
  GT -> Char.Position (xl - yl) xc
  EQ -> Char.Position 0 (xc - yc)
  LT -> Char.Position 0 (xc - charLength rp')
  where
    (_, rp') = splitAtLine xl rp

utf16SubOnRope :: Rope -> Utf16.Position -> Utf16.Position -> Utf16.Position
utf16SubOnRope rp (Utf16.Position xl xc) (Utf16.Position yl yc) = case xl `compare` yl of
  GT -> Utf16.Position (xl - yl) xc
  EQ -> Utf16.Position 0 (xc - yc)
  LT -> Utf16.Position 0 (xc - utf16Length rp')
  where
    (_, rp') = splitAtLine xl rp

charSubOnLines :: Char.TextLines -> Char.Position -> Char.Position -> Char.Position
charSubOnLines tl (Char.Position xl xc) (Char.Position yl yc) = case xl `compare` yl of
  GT -> Char.Position (xl - yl) xc
  EQ -> Char.Position 0 (xc - yc)
  LT -> Char.Position 0 (xc - Char.length tl')
  where
    (_, tl') = Char.splitAtLine xl tl

utf16SubOnLines :: Utf16.TextLines -> Utf16.Position -> Utf16.Position -> Utf16.Position
utf16SubOnLines tl (Utf16.Position xl xc) (Utf16.Position yl yc) = case xl `compare` yl of
  GT -> Utf16.Position (xl - yl) xc
  EQ -> Utf16.Position 0 (xc - yc)
  LT -> Utf16.Position 0 (xc - Utf16.length tl')
  where
    (_, tl') = Utf16.splitAtLine xl tl

-- | Combination of 'splitAtLine' and subsequent 'charSplitAt'.
-- Time is linear in 'Char.posColumn' and logarithmic in 'Char.posLine'.
--
-- >>> :set -XOverloadedStrings
-- >>> charSplitAtPosition (Position 1 0) "f\n𐀀я"
-- ("f\n","𐀀я")
-- >>> charSplitAtPosition (Position 1 1) "f\n𐀀я"
-- ("f\n𐀀","я")
-- >>> charSplitAtPosition (Position 1 2) "f\n𐀀я"
-- ("f\n𐀀я","")
-- >>> charSplitAtPosition (Position 0 2) "f\n𐀀я"
-- ("f\n","𐀀я")
-- >>> charSplitAtPosition (Position 0 3) "f\n𐀀я"
-- ("f\n𐀀","я")
-- >>> charSplitAtPosition (Position 0 4) "f\n𐀀я"
-- ("f\n𐀀я","")
--
charSplitAtPosition :: HasCallStack => Char.Position -> Rope -> (Rope, Rope)
charSplitAtPosition (Char.Position 0 0) = (mempty,)
charSplitAtPosition !len = \case
  Empty -> (Empty, Empty)
  Node l c r _ _ _ _
    | len <= ll -> case charSplitAtPosition len l of
      (before, after)
        | null after -> case charSplitAtPosition len' (c <| r) of
          (r', r'') -> (l <> r', r'')
        | otherwise -> (before, node after c r)
    | len <= llc -> case Char.splitAtPosition len' c of
      (before, after)
        | TL.null after -> case charSplitAtPosition len'' r of
          (r', r'') -> ((l |> c) <> r', r'')
        | otherwise -> (l |> before, after <| r)
    | otherwise -> case charSplitAtPosition len'' r of
      (before, after) -> (node l c before, after)
    where
      ll = charLengthAsPosition l
      lc = Char.lengthAsPosition c
      llc = ll <> lc
      len' = charSubOnRope l len ll
      len'' = charSubOnLines c len' lc

-- | Combination of 'splitAtLine' and subsequent 'utf16SplitAt'.
-- Time is linear in 'Utf16.posColumn' and logarithmic in 'Utf16.posLine'.
--
-- >>> :set -XOverloadedStrings
-- >>> utf16SplitAtPosition (Position 1 0) "f\n𐀀я"
-- Just ("f\n","𐀀я")
-- >>> utf16SplitAtPosition (Position 1 1) "f\n𐀀я"
-- Nothing
-- >>> utf16SplitAtPosition (Position 1 2) "f\n𐀀я"
-- Just ("f\n𐀀","я")
-- >>> utf16SplitAtPosition (Position 0 2) "f\n𐀀я"
-- Just ("f\n","𐀀я")
-- >>> utf16SplitAtPosition (Position 0 3) "f\n𐀀я"
-- Nothing
-- >>> utf16SplitAtPosition (Position 0 4) "f\n𐀀я"
-- Just ("f\n𐀀","я")
--
utf16SplitAtPosition :: HasCallStack => Utf16.Position -> Rope -> Maybe (Rope, Rope)
utf16SplitAtPosition (Utf16.Position 0 0) = Just . (mempty,)
utf16SplitAtPosition !len = \case
  Empty -> Just (Empty, Empty)
  Node l c r _ _ _ _
    | len <= ll -> case utf16SplitAtPosition len l of
      Nothing -> Nothing
      Just (before, after)
        | null after -> case utf16SplitAtPosition len' (c <| r) of
          Nothing -> Nothing
          Just (r', r'') -> Just (l <> r', r'')
        | otherwise -> Just (before, node after c r)
    | len <= llc -> case Utf16.splitAtPosition len' c of
      Nothing -> Nothing
      Just (before, after)
        | Utf16.null after -> case utf16SplitAtPosition len'' r of
          Nothing -> Nothing
          Just (r', r'') -> Just ((l |> c) <> r', r'')
        | otherwise -> Just (l |> before, after <| r)
    | otherwise -> case utf16SplitAtPosition len'' r of
      Nothing -> Nothing
      Just (before, after) -> Just (node l c before, after)
    where
      ll = utf16LengthAsPosition l
      lc = Utf16.lengthAsPosition c
      llc = ll <> lc
      len' = utf16SubOnRope l len ll
      len'' = utf16SubOnLines c len' lc
