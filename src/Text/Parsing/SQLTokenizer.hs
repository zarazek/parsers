{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleContexts #-}

module Text.Parsing.SQLTokenizer (Token(..), significant, sqlTokens, checkedParse,
                                  sqlSymbol, sqlIdentifier, sqlString, withDefault, sqlDecimalNumber, sqlWhitespace,
                                  example1, example2, example3, example4, example5, example6) where

import Data.Attoparsec.Text (Parser, char, inClass, satisfy, digit, string, takeWhile1, parse, IResult(..))
import Control.Applicative (Alternative, (<|>), many, some)
import Data.Foldable (asum)
import Data.List (foldl', intersperse)
import Data.Char (toUpper, isAlpha, isAlphaNum, isSpace, digitToInt)
import Data.Text (Text, pack, unpack, empty)
import Data.Bool (bool)
import qualified Data.ListTrie.Patricia.Map as ST
import Data.ListTrie.Base.Map (WrappedIntMap)

data Token = LeftParen -- (
           | RightParen -- )
           | Comma -- ,
           | Dot -- .
           | Equal -- =
           | AllKeyword
           | AndKeyword
           | AsKeyword
           | AutoincrementKeyword
           | CreateKeyword
           | DefaultKeyword
           | ExistsKeyword
           | FromKeyword
           | IfKeyword
           | IgnoreKeyword
           | InsertKeyword
           | IntoKeyword
           | JoinKeyword
           | KeyKeyword
           | NotKeyword
           | NullKeyword
           | OrKeyword
           | PrimaryKeyword
           | ReferencesKeyword
           | SelectKeyword
           | TableKeyword
           | UnionKeyword
           | UniqueKeyword
           | ValuesKeyword
           | WhereKeyword
           | Identifier Text
           | SqlString Text
           | DecimalNumber Bool Integer Integer Integer
           | HexNumber Integer
           | Whitespace Text
           | LineComment Text
           | BlockComment Text
  deriving (Show)

significant :: Token -> Bool
significant t = case t of
                  Whitespace _   -> False
                  LineComment _  -> False
                  BlockComment _ -> False
                  _              -> True

sqlSymbol :: Parser Token
sqlSymbol = asum (map parseSymbol symbolList)
  where parseSymbol (str, token) = string str *> pure token
        symbolList = [ ("(", LeftParen ),
                       (")", RightParen),
                       (",", Comma     ),
                       (".", Dot       ),
                       ("=", Equal     ) ]

sqlIdentifier :: Parser Token
sqlIdentifier = checkForKeyword <$> ((:) <$> identifierStarter <*> many identifierFollower)
  where identifierStarter = satisfy isAlpha <|> underscore
        identifierFollower = satisfy isAlphaNum <|> underscore
        underscore = char '_'
        checkForKeyword str = case ST.lookup (map toUpper str) keywordMap of
                                Just keyword -> keyword
                                Nothing      -> Identifier (pack str)
        keywordMap :: ST.TrieMap WrappedIntMap Char Token
        keywordMap = foldl' insertKeyword ST.empty keywordList
        insertKeyword m (keyword, token) = ST.insert (map toUpper keyword) token m
        keywordList = [ ("ALL",           AllKeyword          ),
                        ("AND",           AndKeyword          ),
                        ("AS",            AsKeyword           ),
                        ("AUTOINCREMENT", AutoincrementKeyword),
                        ("CREATE",        CreateKeyword       ),
                        ("DEFAULT",       DefaultKeyword      ),
                        ("EXISTS",        ExistsKeyword       ),
                        ("FROM",          FromKeyword         ),
                        ("IF",            IfKeyword           ),
                        ("IGNORE",        IgnoreKeyword       ),
                        ("INSERT",        InsertKeyword       ),
                        ("INTO",          IntoKeyword         ),
                        ("JOIN",          JoinKeyword         ),
                        ("KEY",           KeyKeyword          ),
                        ("NOT",           NotKeyword          ),
                        ("NULL",          NullKeyword         ),
                        ("OR",            OrKeyword           ),
                        ("PRIMARY",       PrimaryKeyword      ),
                        ("REFERENCES",    ReferencesKeyword   ),
                        ("SELECT",        SelectKeyword       ),
                        ("TABLE",         TableKeyword        ),
                        ("UNION",         UnionKeyword        ),
                        ("UNIQUE",        UniqueKeyword       ),
                        ("VALUES",        ValuesKeyword       ),
                        ("WHERE",         WhereKeyword        ) ]

sqlString :: Parser Token
sqlString = (SqlString . pack) <$> (char '\'' *> many sqlStringChar <* char '\'')
  where sqlStringChar = normalChar <|> doubleQuotes
        normalChar = satisfy (/= '\'')
        doubleQuotes = string "''" *> pure '\''

withDefault :: Alternative f => a -> f a -> f a
withDefault x a = a <|> pure x

sqlDecimalNumber :: Parser Token
sqlDecimalNumber = mkSqlDecimal <$> sign <*> floatingPointPart <*> exponentPart
  where mkSqlDecimal s (i, f) e = DecimalNumber s i f e
        sign = withDefault False (isNegative <$> satisfy (inClass "+-"))
        isNegative c = case c of
                        '+' -> False
                        '-' -> True
        floatingPointPart = startsWithDigit <|> startsWithDot
        startsWithDigit = (,) <$> someDigits <*> withDefault 0 (dot *> manyDigits)
        startsWithDot = (0,) <$> (dot *> someDigits)
        exponentPart = withDefault 0 (satisfy (inClass "Ee") *> (mkExponent <$> sign <*> someDigits))
        mkExponent s v = bool 1 (-1) s * v
        someDigits = stringToNumber <$> some digit
        manyDigits = stringToNumber <$> many digit
        dot = char '.'
        stringToNumber = foldl' accum 0
        accum soFar c = 10*soFar + toInteger (digitToInt c)

sqlWhitespace :: Parser Token
sqlWhitespace = Whitespace <$> takeWhile1 isSpace

sqlTokens :: Parser [Token]
sqlTokens = many (sqlIdentifier <|> sqlString <|> sqlDecimalNumber <|> sqlWhitespace <|> sqlSymbol)

checkedParse p str = categorize (parse p str) where
  categorize res = case res of
                     Fail _ ctxs err -> Left (concat (intersperse ", " ctxs) ++ ": " ++ err)
                     Partial f -> categorize (f empty)
                     Done leftover r -> Right (r, leftover)
                                      
example1 :: Text
example1 = "CREATE TABLE IF NOT EXISTS Employees (\
\              login     VARCHAR(8) PRIMARY KEY,\
\              password  VARCHAR(15) NOT NULL,\
\              name      VARCHAR(100) NOT NULL,\
\              active    BOOL NOT NULL DEFAULT TRUE)"

example2 :: Text
example2 = "CREATE TABLE IF NOT EXISTS Tasks (\
\              id          INTEGER PRIMARY KEY AUTOINCREMENT,\
\              title       VARCHAR(100) NOT NULL UNIQUE,\
\              description VARCHAR(1000) NOT NULL,\
\              status      INTEGER NOT NULL DEFAULT 0)"

example3 :: Text
example3 = "CREATE TABLE IF NOT EXISTS EmployeesTasks (\
\              employee          REFERENCES Employees(login),\
\              task              REFERENCES Tasks(id),\
\              assignment_acitve BOOL NOT NULL DEFAULT TRUE,\
\              finished          BOOL NOT NULL DEFAULT FALSE,\
\              time_spent        INTEGER NOT NULL DEFAULT 0,\
\              PRIMARY KEY (employee, task))"

example4 :: Text
example4 = "INSERT OR IGNORE INTO Employees(login, password, name) VALUES\
\             ('ybarodzi', 'pass1', 'Yauheni Barodzich'   ),\
\             ('mlukashe', 'pass2', 'Mikhail Lukashevich' ),\
\             ('tlukashe', 'pass3', 'Tatsiana Lukashevich'),\
\             ('wwisniew', 'pass4', 'Wojciech Wiśniewski' )"

example5 :: Text
example5 = "INSERT OR IGNORE INTO Tasks(title, description) VALUES\
\              ('Pompowanie przedniego koła', 'Zadanie polega na napompowaniu przedniego koła roweru.\nSzybciutko!'),\
\              ('Pompowanie tylnego koła', 'Zadanie polega na napompowaniu tylnego koła roweru.\nPrędziutko!'),\
\              ('Smarowanie łańcucha', 'Zadanie polega na nasmarowaniu łańcucha rowerowego.\nMigiem!')"

example6 :: Text
example6 = "INSERT OR IGNORE INTO EmployeesTasks(employee, task)\
\                     SELECT E.login, T.id\
\                     FROM Employees AS E JOIN Tasks AS T\
\                     WHERE E.name = 'Yauheni Barodzich' AND T.title = 'Pompowanie przedniego koła'\
\           UNION ALL SELECT E.login, T.id\
\                     FROM Employees AS E JOIN Tasks as T\
\                     WHERE E.name = 'Mikhail Lukashevich' AND T.title = 'Pompowanie tylnego koła'\
\           UNION ALL SELECT E.login, T.id\
\                     FROM Employees AS E JOIN Tasks as T\
\                     WHERE E.name = 'Tatsiana Lukashevich' AND T.title = 'Smarowanie łańcucha'\
\           UNION ALL SELECT E.login as employee, T.id AS task\
\                     FROM Employees AS E JOIN Tasks as T\
\                     WHERE E.name = 'Wojciech Wiśniewski' AND T.title = 'Smarowanie łańcucha'"
