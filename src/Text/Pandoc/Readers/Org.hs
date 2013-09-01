{- |
   Module      : Text.Pandoc.Readers.Org
   Copyright   : Copyright (C) 2013 Aliaksey Artamonau
   License     : GNU GPL, version 2 or above

   Maintainer  : Aliaksey Artamonau <aliaksiej.artamonau@gmail.com>
   Stability   : alpha

Conversion from Emacs Org-Mode to 'Pandoc' document.
-}

module Text.Pandoc.Readers.Org where

import Control.Applicative ( (<*), (*>) )

import Data.Default ( def )
import Data.Monoid ( mconcat )
import Data.List ( intersperse )

import Text.Pandoc.Builder ( Blocks, Inlines )
import qualified Text.Pandoc.Builder as Builder
import Text.Pandoc.Definition ( Pandoc )
import Text.Pandoc.Options ( ReaderOptions )
import Text.Pandoc.Parsing  ( Parser, ParserState(..),
                              readWith, anyLine, char, blankline, blanklines,
                              notFollowedBy, eof, manyTill, many1, optional,
                              choice, skipSpaces, try )

-- | Convert Emacs Org-Mode to 'Pandoc' document.
readOrg :: ReaderOptions        -- ^ Reader options.
        -> String               -- ^ String to parse.
        -> Pandoc
readOrg opts = readWith parseOrg def{ stateOptions = opts }

type OrgParser = Parser String ParserState

parseOrg :: OrgParser Pandoc
parseOrg = doc `fmap` manyTill block eof
  where doc = Builder.doc . mconcat

block :: OrgParser Blocks
block = optional blanklines *> choice [ header
                                      , para
                                      ]

header :: OrgParser Blocks
header = do
  level <- length `fmap` try (many1 (char '*') <* char ' ')
  skipSpaces
  Builder.header level `fmap` line

line :: OrgParser Inlines
line = Builder.text `fmap` anyLine

nonBlankLines :: OrgParser Inlines
nonBlankLines = do
  ls <- many1 (notFollowedBy blankline *> skipSpaces *> line)
  return $ mconcat (intersperse Builder.space ls)

para :: OrgParser Blocks
para = Builder.para `fmap` nonBlankLines
