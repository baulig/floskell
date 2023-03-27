{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Check for preprocessor directives and disabled regions.
module Floskell.BlockFilter ( filterDisabledBlocks ) where

import           Data.ByteString.Lazy       ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List                  ( foldl' )

import           Floskell.Types

-- | Current state.
-- | 'Normal' in a normal line.
-- | 'DisabledRegion' means we are inside a "-- floskell-(begin/end)-disabled region pair."
-- | 'ContinuedDirective' means a preprocessor directive such as #enum ended with a
-- | trailing backslash, so it is continued on the next line.
data IterState = Normal | DisabledRegion | ContinuedDirective
    deriving stock ( Enum, Eq, Ord, Show )

-- | State machine record.
data BlockIter = BlockIter { biState    :: IterState
                           , biLines    :: [ByteString]
                           , biComments :: [Comment]
                           }

-- | Public API entry point.
-- | Filter the input lines, removing preprocessor directives and
-- | disabled regions.
filterDisabledBlocks :: [ByteString] -> ([ByteString], [Comment])
filterDisabledBlocks = output . foldl' filterLine empty
  where
    empty = BlockIter Normal mempty mempty

    output iter
        | biState iter /= Normal =
            error "filterDisabledBlocks ends with disabled block."
    output iter = (reverse $ biLines iter, reverse $ biComments iter)

-- | The main filter function.
-- |
-- | We are called via foldl' for each 'line', with the current
-- | state machine 'iter' and return the next 'BlockIter'.
filterLine :: BlockIter -> ByteString -> BlockIter

-- Beginning of an ignored region.
filterLine iter line
    | beginIgnore = let newIter = disableLine iter line
                    in
                        newIter { biState = DisabledRegion }
  where
    beginIgnore = L8.isPrefixOf "-- floskell-begin-disable-region" line

-- End of an ignored region.
filterLine iter line
    | endIgnore =
        let newIter = disableLine iter line in newIter { biState = Normal }
  where
    endIgnore = L8.isPrefixOf "-- floskell-end-disable-region" line

-- Last time we were called, we encountered a preprocessor directive that
-- ended with a backslash, so it continues to the next - this current - line.
-- Check whether we might end with a trailing backslash again in case we are
-- in the middle of a multi-line directive.
filterLine iter line
    | biState iter == ContinuedDirective =
        checkContinuation line $ disableLine iter line

-- We are inside a disabled region.
filterLine iter line
    | biState iter == DisabledRegion = disableLine iter line

-- Preprocessor directive.
filterLine iter line
    | cppLine = checkContinuation line $ disableLine iter line
  where
    cppLine = any (`L8.isPrefixOf` line)
                  [ "#if"
                  , "#end"
                  , "#else"
                  , "#enum"
                  , "#const"
                  , "#define"
                  , "#undef"
                  , "#elif"
                  , "#include"
                  , "#error"
                  , "#warning"
                  ]

-- Just a normal line.
filterLine iter line = iter { biLines = line : biLines iter }

-- | 'filterLine' determined that this line is to be ignored.
disableLine :: BlockIter -> ByteString -> BlockIter
disableLine iter line =
    let pos = length (biLines iter) + length (biComments iter)
        comment = makeComment pos
    in
        iter { biComments = comment : biComments iter }
  where
    makeComment n =
        Comment PreprocessorDirective
                (SrcSpan "" n 1 n (fromIntegral $ L8.length line + 1))
                (L8.unpack line)

-- | Check whether the current 'line' ends with a trailing backslash.
checkContinuation :: ByteString -> BlockIter -> BlockIter
checkContinuation line iter
    | L8.isSuffixOf "\\" line = iter { biState = ContinuedDirective }
checkContinuation _ iter = iter { biState = Normal }
