{-# LANGUAGE OverloadedStrings #-}

import Text.Parsing.Parser (AttoparsecParser, simpleParse)
import Text.Parsing.LineParser (line)
import qualified Text.Parsing.List.Simple as LS
import qualified Text.Parsing.List.StaticInfo as LI
import qualified Text.Parsing.List.Memo as LM
import qualified Text.Parsing.Maybe.Simple as MS
import qualified Text.Parsing.Maybe.StaticInfo as MI
import qualified Text.Parsing.Generic.Simple as GS
import qualified Text.Parsing.Generic.StaticInfo as GI
import qualified Data.Attoparsec.Internal.Types as AI (Parser)
import Data.ByteString.Char8 (ByteString)
import Criterion.Main

lineParsers =  bgroup "line parser" [ bench "list simple"               $ nf (simpleParse (line :: LS.Parser                   ByteString ByteString Int)) str
                                    , bench "list static info"          $ nf (simpleParse (line :: LI.Parser              Char ByteString ByteString Int)) str
                                    , bench "list memoizing"            $ nf (simpleParse (line :: LM.Parser                   ByteString ByteString Int)) str
                                    , bench "maybe simple"              $ nf (simpleParse (line :: MS.Parser                   ByteString ByteString Int)) str
                                    , bench "maybe static info"         $ nf (simpleParse (line :: MI.Parser              Char ByteString ByteString Int)) str
                                    , bench "generic list simple"       $ nf (simpleParse (line :: GS.Parser        []         ByteString ByteString Int)) str
                                    , bench "generic list static info"  $ nf (simpleParse (line :: GI.Parser        []    Char ByteString ByteString Int)) str
                                    , bench "generic maybe simple"      $ nf (simpleParse (line :: GS.Parser        Maybe      ByteString ByteString Int)) str
                                    , bench "generic maybe static info" $ nf (simpleParse (line :: GI.Parser        Maybe Char ByteString ByteString Int)) str
                                    , bench "attoparsec"                $ nf (simpleParse (line :: AttoparsecParser            ByteString ByteString Int)) str ]       
  where str = "2016-Feb-16 12:31:22.526262 user000457: hello from user000457!"

main = defaultMain [ lineParsers ]
