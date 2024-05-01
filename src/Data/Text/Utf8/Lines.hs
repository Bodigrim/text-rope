{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Data.Text.Utf8.Lines
  ( I.TextLines
  , I.fromText
  , I.toText
  , I.null
  -- * Lines
  , I.getLine
  , I.lines
  , I.lengthInLines
  , I.splitAtLine
  -- * UTF-8 code units
  , length
  , splitAt
  , Position(..)
  , lengthAsPosition
  , splitAtPosition
  ) where

import Prelude ((+), (-), seq)
import Control.DeepSeq (NFData, rnf)
import Data.Bool (otherwise)
import Data.Eq (Eq, (==))
import Data.Function ((.), ($))
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import Data.Ord (Ord, (<=), (>), (>=))
import Data.Semigroup (Semigroup(..))
import qualified Data.Text.Array as TA
import Data.Text.Internal (Text(..))
import qualified Data.Text.Lines.Internal as I
import qualified Data.Vector.Unboxed as U
import Data.Word (Word)
import Text.Show (Show)

#if MIN_VERSION_text(2,0,0)
import Data.Bits ((.&.))
#else
import Prelude (fromIntegral)
import Foreign.C.Types (CSize(..))
import GHC.Exts (ByteArray#)
import System.IO (IO)
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Posix.Types (CSsize(..))
#endif

#ifdef DEBUG
import GHC.Stack (HasCallStack)
#else
#define HasCallStack ()
#endif

lengthTextUtf8 :: Text -> Word
#if MIN_VERSION_text(2,0,0)
lengthTextUtf8 (Text _ _ len) = I.intToWord len
#else
lengthTextUtf8 (Text (TA.Array arr) off len) = fromIntegral $ unsafeDupablePerformIO $
  lengthUtf16AsUtf8 arr (fromIntegral off) (fromIntegral len)

foreign import ccall unsafe "_hs_text_lines_length_utf16_as_utf8" lengthUtf16AsUtf8
  :: ByteArray# -> CSize -> CSize -> IO CSsize
#endif

-- | Length in UTF-8 code units aka bytes.
-- Takes linear time.
--
-- >>> :set -XOverloadedStrings
-- >>> length "fя𐀀"
-- 7
-- >>> Data.Text.Lines.length "fя𐀀"
-- 3
--
length :: I.TextLines -> Word
length = lengthTextUtf8 . I.toText

-- | Represent a position in a text.
data Position = Position
  { posLine   :: !Word -- ^ Line.
  , posColumn :: !Word -- ^ Column in UTF-8 code units aka bytes.
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
-- Position {posLine = 0, posColumn = 7}
-- >>> lengthAsPosition "f\n𐀀"
-- Position {posLine = 1, posColumn = 4}
-- >>> lengthAsPosition "f\n𐀀\n"
-- Position {posLine = 2, posColumn = 0}
--
lengthAsPosition
  :: I.TextLines
  -> Position
lengthAsPosition (I.TextLines (Text arr off len) nls) = Position
  { posLine = I.intToWord $ U.length nls
  , posColumn = lengthTextUtf8 $ Text arr nl (off + len - nl)
  }
  where
    nl = if U.null nls then off else U.last nls + 1

splitTextAtUtf8Index :: Word -> Text -> Maybe (Text, Text)
splitTextAtUtf8Index k t@(Text arr off len)
  | k <= 0 = Just (Text arr off 0, t)
#if MIN_VERSION_text(2,0,0)
  | k >= I.intToWord len = Just (t, mempty)
  | otherwise = if c .&. 0xc0 == 0x80 then Nothing else Just
    (Text arr off k', Text arr (off + k') (len - k'))
    where
      k' = I.wordToInt k
      c = TA.unsafeIndex arr (off + k')
#else
  | o >= 0 = Just (Text arr off o, Text arr (off + o) (len - o))
  | otherwise = Nothing
    where
      !(TA.Array arr#) = arr
      o = fromIntegral $ unsafeDupablePerformIO $
        takeUtf16AsUtf8 arr# (fromIntegral off) (fromIntegral len) (fromIntegral k)

foreign import ccall unsafe "_hs_text_lines_take_utf16_as_utf8" takeUtf16AsUtf8
  :: ByteArray# -> CSize -> CSize -> CSize -> IO CSsize
#endif

-- | Combination of 'I.splitAtLine' and subsequent 'splitAt'.
-- If requested number of code units splits a code point in half, return 'Nothing'.
-- Time is linear in 'posColumn', but does not depend on 'posLine'.
--
-- >>> :set -XOverloadedStrings
-- >>> splitAtPosition (Position 1 0) "f\n𐀀я"
-- Just ("f\n","𐀀я")
-- >>> splitAtPosition (Position 1 1) "f\n𐀀я"
-- Nothing
-- >>> splitAtPosition (Position 1 2) "f\n𐀀я"
-- Nothing
-- >>> splitAtPosition (Position 0 2) "f\n𐀀я"
-- Just ("f\n","𐀀я")
-- >>> splitAtPosition (Position 0 3) "f\n𐀀я"
-- Nothing
-- >>> splitAtPosition (Position 0 4) "f\n𐀀я"
-- Nothing
-- >>> splitAtPosition (Position 0 6) "f\n𐀀я"
-- Just ("f\n𐀀","я")
--
splitAtPosition
  :: HasCallStack
  => Position
  -> I.TextLines
  -> Maybe (I.TextLines, I.TextLines)
splitAtPosition (Position line column) (I.TextLines (Text arr off len) nls) =
  case splitTextAtUtf8Index column tx of
    Nothing -> Nothing
    Just (Text _ off' len', tz) -> let n = I.binarySearch nls (off' + len') in Just
      ( I.textLines (Text arr off (off' + len' - off)) (U.take n nls)
      , I.textLines tz (U.drop n nls))
  where
    arrLen = off + len
    nl
      | line <= 0 = off
      | line > I.intToWord (U.length nls) = arrLen
      | otherwise = nls U.! (I.wordToInt line - 1) + 1
    tx = Text arr nl (arrLen - nl)

-- | Split at given UTF-8 code unit aka byte.
-- If requested number of code units splits a code point in half, return 'Nothing'.
-- Takes linear time.
--
-- >>> :set -XOverloadedStrings
-- >>> map (\c -> splitAt c "fя𐀀") [0..7]
-- [Just ("","fя𐀀"),Just ("f","я𐀀"),Nothing,Just ("fя","𐀀"),Nothing,Nothing,Nothing,Just ("fя𐀀","")]
--
splitAt :: HasCallStack => Word -> I.TextLines -> Maybe (I.TextLines, I.TextLines)
splitAt = splitAtPosition . Position 0
