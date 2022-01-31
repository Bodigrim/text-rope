-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Data.Text.Lines.Internal
  ( TextLines(..)
  , fromText
  , null
  -- * Lines
  , lines
  , lengthInLines
  , splitAtLine
  -- * Code points
  , length
  , span
  , splitAt
  , Position(..)
  , lengthAsPosition
  , splitAtPosition
  -- * Utils
  , textLines
  , binarySearch
  , wordToInt
  , intToWord
  ) where

import Prelude ((+), (-), subtract, quot, fromIntegral, seq)
import Control.DeepSeq (NFData, rnf)
import Data.Bits (toIntegralSized)
import Data.Bool (Bool, otherwise)
import Data.Char (Char)
import Data.Eq (Eq, (==))
import Data.Function (on, (.), ($))
import Data.Int (Int)
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import Data.Ord (Ord, compare, (<=), (<), (>))
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import qualified Data.Text.Array as TA
import Data.Text.Internal (Text(..))
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Data.Word (Word)
import Foreign.C.Types (CSize(..))
import GHC.Exts (ByteArray#)
import System.IO (IO)
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Posix.Types (CSsize(..))
import Text.Show (Show, show)

#if MIN_VERSION_text(2,0,0)
#else
import Prelude ((*))
import Data.Bits (shiftR)
#endif

#ifdef DEBUG
import Prelude (error)
import Data.Bool ((&&), not)
import Data.Char (generalCategory, GeneralCategory(..))
import Data.Eq ((/=))
import Data.List ((++))
import Data.Ord ((>=))
import Data.Text.Internal (showText)
import GHC.Stack (HasCallStack)
#else
#define HasCallStack ()
#endif

-- | A wrapper around 'Text' for fast line/column navigation.
-- Concatenation takes linear time.
--
-- This is a building block for 'Data.Text.Rope.Rope',
-- which provides logarithmic concatenation.
data TextLines = TextLines
  { toText     :: !Text
  -- ^ Extract 'Text', O(1).
  , _nlIndices :: !(U.Vector Int)
  }

instance NFData TextLines where
  rnf = (`seq` ())

instance Eq TextLines where
  (==) = (==) `on` toText
  {-# INLINE (==) #-}

instance Ord TextLines where
  compare = compare `on` toText
  {-# INLINE compare #-}

instance Show TextLines where
#ifdef DEBUG
  show (TextLines x y) = "TextLines { " ++ showText x ++ ", " ++ show y ++ " }"
#else
  show = show . toText
  {-# INLINE show #-}
#endif

instance IsString TextLines where
  fromString = fromText . fromString
  {-# INLINE fromString #-}

-- | Create from 'Text', linear time.
fromText :: HasCallStack => Text -> TextLines
fromText t = textLines t (U.fromList $ nlIndices t)
{-# INLINABLE fromText #-}

nlIndices :: Text -> [Int]
#if MIN_VERSION_text(2,0,0)
nlIndices (Text (TA.ByteArray arr#) off len) = go off
  where
    go !n
      | delta < 0 = []
      | otherwise = (n + delta) : go (n + delta + 1)
      where
        delta = fromIntegral $ unsafeDupablePerformIO $
          memchr arr# (fromIntegral n) (fromIntegral (len + off - n))
#else
nlIndices (Text arr off len) = go off
  where
    go !n
      | delta < 0 = []
      | TA.unsafeIndex arr (n + delta) == 0x0A = (n + delta) : go (n + delta + 1)
      | otherwise = go (n + delta + 1)
      where
        delta = fromIntegral (unsafeDupablePerformIO $
          memchr (TA.aBA arr) (2 * fromIntegral n) (2 * fromIntegral (len + off - n))) `shiftR` 1
#endif
{-# INLINABLE nlIndices #-}

foreign import ccall unsafe "_hs_text_lines_memchr0A" memchr
  :: ByteArray# -> CSize -> CSize -> IO CSsize

-- | Check whether a text is empty, O(1).
null :: TextLines -> Bool
null = T.null . toText
{-# INLINE null #-}

instance Semigroup TextLines where
  TextLines t1@(Text _ off1 len1) s1 <> TextLines t2@(Text _ off2 _) s2
    | T.null t1 = textLines t2 s2
    | T.null t2 = textLines t1 s1
    | otherwise = textLines
      (t1 <> t2)
      (U.map (subtract off1) s1 <> U.map (+ (len1 - off2)) s2)
      -- This relies on specific implementation of instance Semigroup Text!
  {-# INLINABLE (<>) #-}
  -- TODO implement sconcat via Builder

instance Monoid TextLines where
  mempty = textLines mempty mempty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}
  -- TODO implement mconcat via Builder

-- | Equivalent to 'Data.List.length' . 'lines', but in O(1).
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
lengthInLines :: TextLines -> Word
lengthInLines (TextLines t nls) = case T.unsnoc t of
  Nothing -> 0
  Just (_, ch) -> intToWord $ U.length nls + (if ch == '\n' then 0 else 1)

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
lines :: TextLines -> [Text]
lines (TextLines (Text arr off len) nls) = go off (U.toList nls)
  where
    arrLen = off + len
    go i [] = [Text arr i (arrLen - i) | i < arrLen]
    go i (x : xs) = Text arr i (x - i) : go (x + 1) xs
{-# INLINABLE lines #-}

-- | Split at given line, O(1).
--
-- >>> :set -XOverloadedStrings
-- >>> map (\l -> splitAtLine l "foo\nbar") [0..3]
-- [("","foo\nbar"),("foo\n","bar"),("foo\nbar",""),("foo\nbar","")]
--
splitAtLine :: HasCallStack => Word -> TextLines -> (TextLines, TextLines)
splitAtLine k = splitAtPosition (Position k 0)
{-# INLINE splitAtLine #-}

-------------------------------------------------------------------------------
-- Unicode code points

-- | Length in code points, similar to @Data.Text.@'Data.Text.length'.
-- Takes linear time.
--
-- >>> :set -XOverloadedStrings
-- >>> length "fя𐀀"
-- 3
-- >>> Data.Text.Utf16.Lines.length "fя𐀀"
-- 4
--
length :: TextLines -> Word
length = intToWord . T.length . toText
{-# INLINE length #-}

-- | Represent a position in a text.
data Position = Position
  { posLine   :: !Word -- ^ Line.
  , posColumn :: !Word -- ^ Column in code points.
  } deriving (Eq, Ord, Show)

instance NFData Position where
  rnf = (`seq` ())

-- | Associativity does not hold when 'posLine' overflows.
instance Semigroup Position where
  Position l1 c1 <> Position l2 c2 =
    Position (l1 + l2) (if l2 == 0 then c1 + c2 else c2)

instance Monoid Position where
  mempty = Position 0 0
  mappend = (<>)

-- | Measure text length as an amount of lines and columns.
-- Time is proportional to the length of the last line.
--
-- >>> :set -XOverloadedStrings
-- >>> lengthAsPosition "f𐀀"
-- Position {posLine = 0, posColumn = 2}
-- >>> lengthAsPosition "f\n𐀀"
-- Position {posLine = 1, posColumn = 1}
-- >>> lengthAsPosition "f\n𐀀\n"
-- Position {posLine = 2, posColumn = 0}
--
lengthAsPosition
  :: TextLines
  -> Position
lengthAsPosition (TextLines (Text arr off len) nls) = Position
  { posLine   = intToWord $ U.length nls
  , posColumn = intToWord $ T.length $ Text arr nl (off + len - nl)
  }
  where
    nl = if U.null nls then off else U.last nls + 1
{-# INLINABLE lengthAsPosition #-}

-- | Span by a predicate, similar to @Data.Text.@'Data.Text.span'.
-- Takes linear (by length of the prefix satisfying the predicate) time.
span
  :: HasCallStack
  => (Char -> Bool)
  -> TextLines
  -> (TextLines, TextLines)
span f tl@(TextLines tx@(Text arr off _) nls)
  | len' == 0 = (mempty, tl)
  | otherwise = (y, z)
  where
    (Text _ off' len', tz) = T.span f tx
    -- This assumes that offset is the same as in tx
    n = binarySearch nls (off' + len')
    y = textLines (Text arr off (off' + len' - off)) (U.take n nls)
    z = textLines tz (U.drop n nls)
{-# INLINABLE span #-}

-- | Combination of 'splitAtLine' and subsequent 'splitAt'.
-- Time is linear in 'posColumn', but does not depend on 'posLine'.
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
splitAtPosition
  :: HasCallStack
  => Position
  -> TextLines
  -> (TextLines, TextLines)
splitAtPosition (Position line column) (TextLines (Text arr off len) nls) = (y, z)
  where
    arrLen = off + len
    nl
      | line <= 0 = off
      | line > intToWord (U.length nls) = arrLen
      | otherwise = nls U.! (wordToInt line - 1) + 1
    tx = Text arr nl (arrLen - nl)
    (Text _ off' len', tz)
      | column <= 0 = (Text arr nl 0, tx)
      | otherwise = case toIntegralSized column of
        Nothing -> (tx, mempty)
        Just column' -> T.splitAt column' tx
    -- This assumes that offset is the same as in tx
    n = binarySearch nls (off' + len')
    y = textLines (Text arr off (off' + len' - off)) (U.take n nls)
    z = textLines tz (U.drop n nls)
{-# INLINABLE splitAtPosition #-}

-- | Split at given code point, similar to @Data.Text.@'Data.Text.splitAt'.
-- Takes linear time.
--
-- >>> :set -XOverloadedStrings
-- >>> map (\c -> splitAt c "fя𐀀") [0..4]
-- [("","fя𐀀"),("f","я𐀀"),("fя","𐀀"),("fя𐀀",""),("fя𐀀","")]
--
splitAt :: HasCallStack => Word -> TextLines -> (TextLines, TextLines)
splitAt = splitAtPosition . Position 0
{-# INLINE splitAt #-}

-------------------------------------------------------------------------------
-- Utils

binarySearch
  :: (Ord a, U.Unbox a)
  => U.Vector a
  -> a
  -> Int
binarySearch vec el
  | U.null vec = 0
  | el <= U.head vec = 0
  | U.last vec < el = U.length vec
  | otherwise = go 0 (U.length vec - 1)
  where
    go i j
      | i + 1 == j = j
      | vec U.! k < el = go k j
      | otherwise = go i k
      where
        k = (i + j) `quot` 2
{-# SPECIALIZE binarySearch :: U.Vector Int -> Int -> Int #-}
{-# INLINABLE binarySearch #-}

intToWord :: Int -> Word
intToWord = fromIntegral
{-# INLINE intToWord #-}

wordToInt :: Word -> Int
wordToInt = fromIntegral
{-# INLINE wordToInt #-}

-------------------------------------------------------------------------------
-- Debug

#ifdef DEBUG

isValid :: TextLines -> Bool
isValid (TextLines t@(Text arr off len) stops) =
  not containsSurrogates && len >= 0 && go off (U.toList stops)
  where
    arrLen = off + len
    go i [] = T.all (/= '\n') (Text arr i (arrLen - i))
    go i (x : xs) = i <= x
                 && T.all (/= '\n') (Text arr i (x - i))
                 && T.head (Text arr x (arrLen - x)) == '\n'
                 && go (x + 1) xs
    containsSurrogates = T.any ((== Surrogate) . generalCategory) t

textLines :: HasCallStack => Text -> U.Vector Int -> TextLines
textLines x y
  | isValid t = t
  | otherwise = error $ "Data.Text.Lines: violated internal invariant in " ++ show t
  where
    t = TextLines x y

#else

textLines :: HasCallStack => Text -> U.Vector Int -> TextLines
textLines = TextLines
{-# INLINE textLines #-}

#endif
