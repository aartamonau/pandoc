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
import Data.Monoid ( mconcat, mempty, (<>) )

import Text.Pandoc.Builder ( Blocks, Inlines )
import qualified Text.Pandoc.Builder as Builder
import Text.Pandoc.Definition ( Pandoc(..) )
import Text.Pandoc.Options ( ReaderOptions )
import Text.Pandoc.Parsing  ( Parser, ParserState(..),
                              getState, updateState, stateMeta,
                              readWith, anyLine, char, blankline, blanklines,
                              notFollowedBy, eof, manyTill, many1,
                              optional, choice, skipSpaces, try, string,
                              newline, anyChar, lookAhead,
                              spaceChar, nonspaceChar, enclosed, oneOf )

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
               , commentLine *> pure mempty -- should go before para
               , para
               , blanklines *> pure mempty
               ]

header :: OrgParser Blocks
header = do
  level <- length <$> try (many1 (char '*') <* char ' ')
  skipSpaces
  Builder.header level <$> textLine

inline :: OrgParser Inlines
inline = choice [ whitespace
                , enclosure
                , danglingEnclosure
                , word
                ]

whitespace :: OrgParser Inlines
whitespace = do
  spaceChar *> skipSpaces

  -- don't return space in the end of lines
  (lookAhead newline *> pure mempty) <|> pure Builder.space

inlineSep :: OrgParser ()
inlineSep = (spaceChar <|> newline) *> pure ()

word :: OrgParser Inlines
word = Builder.str <$> many1 (notFollowedBy end *> anyChar)
  where end = skip inlineSep <|> skip danglingEnclosure

enclosureSpecs :: [(Char, OrgParser Inlines)]
enclosureSpecs = [ spec '/' (fmap Builder.emph . enclWords)
                   -- treat underlines as emphasis
                 , spec '_' (fmap Builder.emph . enclWords)
                 , spec '*' (fmap Builder.strong . enclWords)
                 , spec '+' (fmap Builder.strikeout . enclWords)
                 , spec '~' code
                 , spec '=' code
                 ]
  where encl :: Char -> OrgParser a -> OrgParser [a]
        encl c = enclosed start end
          where start = char c
                          <* notFollowedBy (skip inlineSep <|> skip (char c))
                end = char c <* lookAhead inlineSep

        enclWords :: Char -> OrgParser Inlines
        enclWords c = mconcat <$> encl c (nonWhitespace
                                          <|> whitespaceNonWhitespace)
          where nonWhitespace = word <|> danglingEnclosure
                whitespaceNonWhitespace = do
                  wp <- whitespace
                  nwp <- nonWhitespace
                  return $ wp <> nwp

        code :: Char -> OrgParser Inlines
        code c = Builder.code <$> concat
                              <$> encl c (nonWhitespace
                                          <|> whitespaceNonWhitespace)
          where dangling = (:[]) <$> char c <* lookAhead inlineSep
                nonWhitespace = many1 (notFollowedBy dangling *> nonspaceChar)
                                <|> dangling

                whitespaceNonWhitespace = do
                  wp <- many1 spaceChar
                  nwp <- nonWhitespace

                  return $ wp ++ nwp

        spec :: Char -> (Char -> a) -> (Char, a)
        spec c f = (c, f c)

enclosure :: OrgParser Inlines
enclosure = choice $ map snd enclosureSpecs

danglingEnclosure :: OrgParser Inlines
danglingEnclosure = Builder.str <$> (:[]) <$> try p
  where chars = map fst enclosureSpecs
        p = oneOf chars <* lookAhead inlineSep

textLine :: OrgParser Inlines
textLine = mconcat <$> manyTill inline end
  where end = skip commentLine <|> skip newline

metaLine :: OrgParser Inlines
metaLine = do
  try $ string "#+"
  field <- map toLower <$> manyTill anyChar (lookAhead $ newline <|> char ':')

  optional $ char ':'

  skipSpaces
  rest <- anyLine

  let value | null rest = mempty
            | otherwise = Builder.text rest

  updateState $ Builder.setMeta field value
  return mempty

commentLine :: OrgParser Inlines
commentLine = try (skipSpaces *> string "# ") *> anyLine *> pure mempty

para :: OrgParser Blocks
para = do
  ls <- intersperse Builder.space <$> filter (/= mempty) <$> many1 paraLine
  return $ Builder.para (mconcat ls)

  where paraLine = notFollowedBy paraEnd *> line
          where line = metaLine <|> (skipSpaces *> textLine)
        paraEnd = skip blankline <|> skip header

skip :: OrgParser a -> OrgParser ()
skip p = p *> pure ()
