{- |
   Module      : Text.Pandoc.Readers.Org
   Copyright   : Copyright (C) 2013 Aliaksey Artamonau
   License     : GNU GPL, version 2 or above

   Maintainer  : Aliaksey Artamonau <aliaksiej.artamonau@gmail.com>
   Stability   : alpha

Conversion from Emacs Org-Mode to 'Pandoc' document.
-}

module Text.Pandoc.Readers.Org where

import Control.Applicative ( (<$>), (<*), (*>), (<|>), pure )

import Data.Char ( toLower )
import Data.Default ( def )
import Data.List ( intersperse )
import Data.Monoid ( mconcat, mempty )

import Text.Pandoc.Builder ( Blocks, Inlines )
import qualified Text.Pandoc.Builder as Builder
import Text.Pandoc.Definition ( Pandoc(..) )
import Text.Pandoc.Options ( ReaderOptions )
import Text.Pandoc.Parsing  ( Parser, ParserState(..),
                              getState, updateState, stateMeta,
                              readWith, anyLine, char, blankline, blanklines,
                              notFollowedBy, eof, manyTill, many1,
                              optional, choice, skipSpaces, try, string,
                              newline, anyChar, lookAhead )

-- | Convert Emacs Org-Mode to 'Pandoc' document.
readOrg :: ReaderOptions        -- ^ Reader options.
        -> String               -- ^ String to parse.
        -> Pandoc
readOrg opts s = readWith parseOrg def{ stateOptions = opts } (s ++ "\n")

type OrgParser = Parser String ParserState

parseOrg :: OrgParser Pandoc
parseOrg = do
  bs <- mconcat <$> manyTill block eof
  meta <- stateMeta <$> getState

  let Pandoc _ bs' = Builder.doc bs

  return $ Pandoc meta bs'

block :: OrgParser Blocks
block = choice [ header
               , metaLine *> pure mempty -- should go before para
               , para
               , blanklines *> pure mempty
               ]

header :: OrgParser Blocks
header = do
  level <- length `fmap` try (many1 (char '*') <* char ' ')
  skipSpaces
  Builder.header level `fmap` textLine

textLine :: OrgParser Inlines
textLine = Builder.text `fmap` anyLine

metaLine :: OrgParser ()
metaLine = do
  try $ string "#+"
  field <- map toLower <$> manyTill anyChar (lookAhead $ newline <|> char ':')

  optional $ char ':'

  skipSpaces
  rest <- anyLine

  let value | null rest = mempty
            | otherwise = Builder.text rest

  updateState $ Builder.setMeta field value

para :: OrgParser Blocks
para = do
  ls <- intersperse Builder.space `fmap` many1 paraLine
  return $ Builder.para (mconcat ls)

  where paraLine = notFollowedBy paraEnd *> line
          where line = (metaLine *> pure mempty) <|> (skipSpaces *> textLine)
        paraEnd = skip blankline <|> skip header

skip :: OrgParser a -> OrgParser ()
skip p = p *> pure ()
