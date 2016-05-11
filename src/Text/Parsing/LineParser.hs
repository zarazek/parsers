{-# LANGUAGE OverloadedStrings #-}

module Text.Parsing.LineParser (line) where

import Text.Parsing.Parser
import Data.String (IsString)
import Control.Monad (MonadPlus, mzero, mfilter)
import Data.Foldable (asum)
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.LocalTime (TimeOfDay, LocalTime(..), makeTimeOfDayValid)
import Data.Fixed (resolution, Fixed(..), Uni, Micro, Pico)


nothingToEmpty :: MonadPlus m => m (Maybe a) -> m a
nothingToEmpty act = act >>= maybe mzero return

labeledDecimal :: (Parser p lbl lst, IsString lbl) => p lbl lst Integer
labeledDecimal = label "decimal" decimal

rangeDecimal :: (Parser p lbl lst, IsString lbl, Integral a) => Integer -> Integer -> p lbl lst a
rangeDecimal min max = fromInteger <$> mfilter (\n -> min <= n && n <= max) labeledDecimal

date :: (Parser p lst lst, IsString lst) => p lst lst Day
date = label "date" $ nothingToEmpty (fromGregorianValid <$> year <* dash <*> month <* dash <*> day)
  where year = label "year" labeledDecimal
        dash = label "dash" $ char '-'
        month = label "month" $ asum $ zipWith toParser [1..12] months
        toParser num str = label str (string str *> pure num)
        months =  ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
        day = label "day" $ rangeDecimal 1 31

time :: (Parser p lbl lst, IsString lbl) => p lbl lst TimeOfDay
time = label "time" $ nothingToEmpty (makeTimeOfDayValid <$> hour <* colon <*> minute <* colon <*> seconds)
  where hour = label "hour" $ rangeDecimal 0 23
        minute = label "minute" $ rangeDecimal 0 59
        colon = label "colon" $ char ':'
        seconds = label "seconds" $ toPicoseconds <$> second <* dot <*> microsecond
        toPicoseconds sec usec = MkFixed (secFactor*sec + usecFactor*usec)
        secFactor = picoResolution `div` resolution (undefined :: Uni)
        usecFactor = picoResolution `div` resolution (undefined :: Micro)
        picoResolution = resolution (undefined :: Pico)
        second = label "second" $ rangeDecimal 0 59
        dot = char '.'
        microsecond = label "microsecond" $ rangeDecimal 0 999999

dateTime :: (Parser p lst lst, IsString lst) => p lst lst LocalTime
dateTime = label "dateTime" $ LocalTime <$> date <* space <*> time
  where space = label "space" $ char ' '

line :: (Parser p lst lst, IsString lst) => p lst lst Int
line = label "line" $
       fmap fst $
       mfilter (uncurry (==)) $
       (,) <$> (dateTime *> str1 *> userId <* str2) <*> userId <* exclamationMark
  where str1 = label "str1" $ string " user"
        userId = label "userId" $ fromInteger <$> labeledDecimal
        str2 = label "str2" $ string ": hello from user"
        exclamationMark = label "exclamationMark" $ char '!'

