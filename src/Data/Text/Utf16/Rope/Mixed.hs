-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Data.Text.Utf16.Rope.Mixed
  ( Mixed.Rope
  , Mixed.fromText
  , Mixed.fromTextLines
  , Mixed.toText
  , Mixed.toTextLines
  , Mixed.null
  -- * Lines
  , Mixed.lines
  , Mixed.lengthInLines
  , Mixed.splitAtLine
  -- * Code points
  , Mixed.charLength
  , Mixed.charSplitAt
  , Mixed.charLengthAsPosition
  , Mixed.charSplitAtPosition
  -- * UTF-16 code units
  , Mixed.utf16Length
  , Mixed.utf16SplitAt
  , Mixed.utf16LengthAsPosition
  , Mixed.utf16SplitAtPosition
  ) where

import qualified Data.Text.Mixed.Rope as Mixed
