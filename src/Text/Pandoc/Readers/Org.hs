{- |
   Module      : Text.Pandoc.Readers.Org
   Copyright   : Copyright (C) 2013 Aliaksey Artamonau
   License     : GNU GPL, version 2 or above

   Maintainer  : Aliaksey Artamonau <aliaksiej.artamonau@gmail.com>
   Stability   : alpha

Conversion from Emacs Org-Mode to 'Pandoc' document.
-}

module Text.Pandoc.Readers.Org where

import Control.Applicative ( (*>), (<|>), many )
import Data.Default ( def )
import Data.Monoid ( mconcat )

import Text.Pandoc.Builder ( Blocks, Inlines )
import qualified Text.Pandoc.Builder as Builder
import Text.Pandoc.Definition ( Pandoc )
import Text.Pandoc.Options ( ReaderOptions )
import Text.Pandoc.Parsing  ( Parser, ParserState(..),
                              readWith, anyLine, char )

-- | Convert Emacs Org-Mode to 'Pandoc' document.
readOrg :: ReaderOptions        -- ^ Reader options.
        -> String               -- ^ String to parse.
        -> Pandoc
readOrg opts = readWith parseOrg def{ stateOptions = opts }

type OrgParser = Parser String ParserState

parseOrg :: OrgParser Pandoc
parseOrg = doc `fmap` many header
  where doc = Builder.doc . mconcat

header :: OrgParser Blocks
header = char '*' *> go 1
  where go level = (char '*' *> go (level + 1)) <|>
                   Builder.header level `fmap` line

line :: OrgParser Inlines
line = Builder.text `fmap` anyLine
