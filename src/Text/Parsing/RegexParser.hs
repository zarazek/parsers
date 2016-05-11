{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Text.Parsing.RegexParser (Regex(..), regex, fasterRegex) where

import Text.Parsing.Parser
import Control.Applicative ((<|>), many, some)
import Control.Monad (guard)
import Control.DeepSeq (NFData(..))
import Data.List ((\\))
import Data.String (IsString)

data Regex = Epsilon
           | Any
           | Letter Char
           | Set [Char]
           | Star Regex
           | Plus Regex
           | Concatenation Regex Regex
           | Alternative Regex Regex
           | Repetition Regex Int Int
  deriving (Eq, Show)

instance NFData Regex where
  rnf expr = case expr of
               Epsilon              -> ()
               Any                  -> ()
               Letter c             -> c `seq` ()
               Set lst              -> rnf lst
               Star e               -> rnf e
               Plus e               -> rnf e
               Concatenation e1 e2  -> rnf e1 `seq` rnf e2
               Alternative e1 e2    -> rnf e1 `seq` rnf e2
               Repetition e min max -> min `seq` max `seq` rnf e

letters = ['A'..'Z'] ++ ['a'..'z']
digits = ['0'..'9']
other = "~!@#$%^&-_=;:<>/? "
quotes = "\"'`"
metaCharacters = "|*+{,}()[].\\"
ordinaryCharacters = letters ++ digits ++ other
quotedCharacters =  metaCharacters ++ quotes
alphabet = ordinaryCharacters ++ quotedCharacters
rangeMetaCharacters = "^[]-\\"
rangeQuotedCharacters = rangeMetaCharacters ++ quotes
rangeOrdinaryCharacters = alphabet \\ rangeQuotedCharacters

regex :: (Parser p lbl lst, IsString lbl) => p lbl lst Regex
regex = regex'
  where regex' = label "regex" $ union <|> simpleRegex
        union = label "union" $ Alternative <$> simpleRegex <* char '|' <*> regex'
        simpleRegex = label "simpleRegex" $ concatenation <|> basicRegex
        concatenation = label "concatenation" $ Concatenation <$> basicRegex <*> simpleRegex
        basicRegex = label "basicRegex" $ star <|> plus  <|> exactRepetition <|> rangeRepetition <|> elementaryRegex
        star = label "star" $ Star <$> elementaryRegex <* char '*'
        plus = label "plus" $ Plus <$> elementaryRegex <* char '+'
        exactRepetition = label "exactRepetition" $ makeRepetition <$> elementaryRegex <* openBrace <*> cnt <* closeBrace
        makeRepetition expr count = Repetition expr count count
        cnt = label "cnt" $ fromInteger <$> decimal
        rangeRepetition = label "rangeRepetition" $ do
          res <- parse
          guard (check res)
          return res
        parse = Repetition <$> elementaryRegex <* openBrace <*> cnt <* comma <*> cnt <* closeBrace
        check (Repetition _ min max) = min < max
        openBrace = label "openBrace" $ char '{'
        closeBrace = label "closeBrace" $ char '}'
        comma = label "comma" $ char ','
        elementaryRegex = label "elementaryRegex" $ group <|> set <|> anyR <|> charR
        group = label "group" $ char '(' *> regex' <* char ')'
        set = label "set" $ char '[' *> setSpec <* char ']'
        setSpec = label "setSpec" $ Set <$> (negate <$> (char '^' *> specElements) <|> specElements)
        negate s = alphabet \\ s
        specElements = label "specElements" $ concat <$> some specElement
        specElement = label "specElement" $ singleCharElement <|> rangeElement
        singleCharElement = label "singleCharElement" $ singleton <$> setChar
        singleton x = [x]
        rangeElement = label "rangeElement" $ enumFromTo <$> setChar <* char '-' <*> setChar
        setChar = label "setChar" $ ordinarySetChar <|> escapedSetChar
        ordinarySetChar = label "ordinarySetChar" $ oneOf rangeOrdinaryCharacters
        escapedSetChar = label "escapedSetChar" $ char '\\' *> oneOf rangeQuotedCharacters
        anyR = label "anyR" $ char '.' *> pure Any
        charR = label "charR" $ ordinaryChar <|> escapedChar
        ordinaryChar = label "ordinaryChar" $ Letter <$> oneOf ordinaryCharacters
        escapedChar = label "escapedChar" $ Letter <$> (char '\\' *> oneOf quotedCharacters)

fromJust (Just x) = x

fasterRegex :: (Parser p lbl lst, IsString lbl) => p lbl lst Regex
fasterRegex = regex'
  where regex' = label "regex" $ mkAlternative <$> simpleRegex <*> many (char '|' *> simpleRegex)
        mkAlternative head tail = buildExpr Alternative (head:tail)
        buildExpr cons = fromJust . foldr f Nothing
          where f expr  Nothing      = Just expr
                f expr1 (Just expr2) = Just (cons expr1 expr2)
        simpleRegex = label "simpleRegex" $ mkConcatenation <$> some basicRegex
        mkConcatenation = buildExpr Concatenation
        basicRegex = label "basicRegex" $ star <|> plus  <|> exactRepetition <|> rangeRepetition <|> elementaryRegex
        star = label "star" $ Star <$> elementaryRegex <* char '*'
        plus = label "plus" $ Plus <$> elementaryRegex <* char '+'
        exactRepetition = label "exactRepetition" $ makeRepetition <$> elementaryRegex <* openBrace <*> cnt <* closeBrace
        makeRepetition expr count = Repetition expr count count
        cnt = label "cnt" $ fromInteger <$> decimal
        rangeRepetition = label "rangeRepetiion" $ do
          res <- parse
          guard (check res)
          return res
        parse = Repetition <$> elementaryRegex <* openBrace <*> cnt <* comma <*> cnt <* closeBrace
        check (Repetition _ min max) = min < max
        openBrace = label "openBrace" $ char '{'
        closeBrace = label "closeBrace" $ char '}'
        comma = label "comma" $ char ','
        elementaryRegex = label "elementaryRegex" $ group <|> set <|> anyR <|> charR
        group = label "group" $ char '(' *> regex' <* char ')'
        set = label "set" $ char '[' *> setSpec <* char ']'
        setSpec = label "setSpec" $ Set <$> (negate <$> (char '^' *> specElements) <|> specElements)
        negate s = alphabet \\ s
        specElements = label "specElements" $ concat <$> some specElement
        specElement = label "specElement" $ singleCharElement <|> rangeElement
        singleCharElement = label "singleCharElement" $ singleton <$> setChar
        singleton x = [x]
        rangeElement = label "rangeElement" $ enumFromTo <$> setChar <* char '-' <*> setChar
        setChar = label "setChar" $ ordinarySetChar <|> escapedSetChar
        ordinarySetChar = label "ordinarySetChar" $ oneOf rangeOrdinaryCharacters
        escapedSetChar = label "escapedSetChar" $ char '\\' *> oneOf rangeQuotedCharacters
        anyR = label "anyR" $ char '.' *> pure Any
        charR = label "charR" $ ordinaryChar <|> escapedChar
        ordinaryChar = label "ordinaryChar" $ Letter <$> oneOf ordinaryCharacters
        escapedChar = label "escapedChar" $ Letter <$> (char '\\' *> oneOf quotedCharacters)
