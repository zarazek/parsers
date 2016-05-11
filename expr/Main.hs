{-# LANGUAGE BangPatterns #-}

import Prelude hiding (getLine, filter, putStrLn)
import Text.Parsing.Parser (char, decimal, simpleParse)
import qualified Text.Parsing.List.Simple as LS
import qualified Text.Parsing.List.StaticInfo as LI
import qualified Text.Parsing.List.Memo as LM
import qualified Text.Parsing.List.Combined as LC
import qualified Text.Parsing.Maybe.Simple as MS
import qualified Text.Parsing.Maybe.StaticInfo as MI
import qualified Text.Parsing.Generic.Simple as GS
import qualified Text.Parsing.Generic.StaticInfo as GI
import qualified Data.Attoparsec.Internal.Types as AI (Parser)
import Data.ByteString.Char8 (ByteString, getLine, filter, putStrLn)
import Control.Applicative ((<|>), many)
import Data.Bits (testBit, shiftR)
import Data.Maybe (fromJust)

data Expression = Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Neg Expression
                | Num Int
  deriving (Show)
  
data ExpressionOp = Plus | Minus
data TermOp = Multiplication | Division

expressionP :: LS.Parser a ByteString Expression
expressionP = mkExpression <$> many ((,) <$> termP <*> expressionOpP) <*> termP
  where mkExpression list expr = foldr f expr list
          where f (expr, op) soFar = case op of
                                       Plus  -> Add expr soFar
                                       Minus -> Sub expr soFar
        expressionOpP = plusP <|> minusP
        plusP = char '+' *> pure Plus
        minusP = char '-' *> pure Minus
        termP = mkTerm <$> many ((,) <$> factorP <*> termOpP) <*> factorP
        mkTerm list expr = foldr f expr list
          where f (expr, op) soFar = case op of
                                       Multiplication -> Mul expr soFar
                                       Division       -> Div expr soFar
        termOpP = multiplicationP <|> divisionP
        multiplicationP = char '*' *> pure Multiplication
        divisionP = char '/' *> pure Division
        factorP = numP <|> posP <|> negP <|> parenthP
        numP = (Num . fromInteger) <$> decimal
        posP = char '+' *> factorP
        negP = Neg <$> (char '-' *> factorP)
        parenthP = char '(' *> expressionP <* char ')'

eval expr = case expr of
              Add ex1 ex2 -> (eval ex1 + eval ex2) `mod` n
              Sub ex1 ex2 -> (eval ex1 - eval ex2) `mod` n
              Mul ex1 ex2 -> (eval ex1 * eval ex2) `mod` n
              Div ex1 ex2 -> (eval ex1 * pow (eval ex2) (n-2)) `mod` n
              Neg ex -> (- eval ex) `mod` n
              Num k -> k
  where n = 1000000007
        pow base exp = go exp base 1
          where go !exp !base !soFar = case exp of
                                      0 -> soFar
                                      k -> case testBit exp 0 of
                                             True  -> go reducedExp squaredBase (soFar*base `mod` n)
                                             False -> go reducedExp squaredBase soFar
                                           where reducedExp = exp `shiftR` 1
                                                 squaredBase = (base * base) `mod` n

fromRight (Right b) = b

main = do
  line <- getLine
  let trimmedLine = filter (/= ' ') line
  let expr = fromJust (simpleParse expressionP trimmedLine)
  print (eval expr)
