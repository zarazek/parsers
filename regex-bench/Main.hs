{-# LANGUAGE OverloadedStrings #-}

import Text.Parsing.Parser (AttoparsecParser, simpleParse)
import Text.Parsing.RegexParser (Regex(..), regex, fasterRegex)
import qualified Text.Parsing.List.Simple as LS
import qualified Text.Parsing.List.StaticInfo as LI
import qualified Text.Parsing.List.Memo as LM
import qualified Text.Parsing.List.Combined as LC
import qualified Text.Parsing.List.NewMemo as LN
import qualified Text.Parsing.Maybe.Simple as MS
import qualified Text.Parsing.Maybe.StaticInfo as MI
import qualified Text.Parsing.Generic.Simple as GS
import qualified Text.Parsing.Generic.StaticInfo as GI
import qualified Text.Parsing.Generic.MostGeneric as GM
import qualified Data.Attoparsec.Internal.Types as AI (Parser)
import Data.ByteString.Char8 (ByteString, pack)
import Criterion.Main

regexParser :: Int -> ByteString -> Benchmark
regexParser id txt = bgroup name [ bench "list simple"                    $ nf (simpleParse (regex       :: LS.Parser                   String ByteString Regex)) txt
                                 , bench "list static info"               $ nf (simpleParse (regex       :: LI.Parser              Char String ByteString Regex)) txt
                                 , bench "list memoization"               $ nf (simpleParse (regex       :: LM.Parser                   String ByteString Regex)) txt
                                 , bench "list combined"                  $ nf (simpleParse (regex       :: LC.Parser              Char String ByteString Regex)) txt
                                 , bench "list free simple"               $ nf (simpleParse (regex       :: LN.Parser              Char String ByteString Regex)) txt
                                 , bench "maybe simple"                   $ nf (simpleParse (regex       :: MS.Parser                   String ByteString Regex)) txt
                                 , bench "maybe static info"              $ nf (simpleParse (regex       :: MI.Parser              Char String ByteString Regex)) txt
                                 , bench "generic list simple"            $ nf (simpleParse (regex       :: GS.Parser        []         String ByteString Regex)) txt
                                 , bench "generic list static info"       $ nf (simpleParse (regex       :: GI.Parser        []    Char String ByteString Regex)) txt
                                 , bench "generic maybe simple"           $ nf (simpleParse (regex       :: GS.Parser        Maybe      String ByteString Regex)) txt
                                 , bench "generic maybe static info"      $ nf (simpleParse (regex       :: GI.Parser        Maybe Char String ByteString Regex)) txt
                                 , bench "most generic maybe"             $ nf (simpleParse (regex       :: GM.Parser              Char String ByteString Regex)) txt
                                 , bench "attoparsec"                     $ nf (simpleParse (regex       :: AttoparsecParser            String ByteString Regex)) txt
                                 , bench "fast list simple"               $ nf (simpleParse (fasterRegex :: LS.Parser                   String ByteString Regex)) txt
                                 , bench "fast list static info"          $ nf (simpleParse (fasterRegex :: LI.Parser              Char String ByteString Regex)) txt
                                 , bench "fast list memoization"          $ nf (simpleParse (fasterRegex :: LM.Parser                   String ByteString Regex)) txt
                                 , bench "fast list combined"             $ nf (simpleParse (fasterRegex :: LC.Parser              Char String ByteString Regex)) txt
                                 , bench "fast list free simple"          $ nf (simpleParse (fasterRegex :: LN.Parser              Char String ByteString Regex)) txt
                                 , bench "fast maybe simple"              $ nf (simpleParse (fasterRegex :: MS.Parser                   String ByteString Regex)) txt
                                 , bench "fast maybe static info"         $ nf (simpleParse (fasterRegex :: MI.Parser              Char String ByteString Regex)) txt
                                 , bench "fast generic list simple"       $ nf (simpleParse (fasterRegex :: GS.Parser        []         String ByteString Regex)) txt
                                 , bench "fast generic list static info"  $ nf (simpleParse (fasterRegex :: GI.Parser        []    Char String ByteString Regex)) txt
                                 , bench "fast generic maybe simple"      $ nf (simpleParse (fasterRegex :: GS.Parser        Maybe      String ByteString Regex)) txt
                                 , bench "fast generic maybe static info" $ nf (simpleParse (fasterRegex :: GI.Parser        Maybe Char String ByteString Regex)) txt
                                 , bench "fast attoparsec"                $ nf (simpleParse (fasterRegex :: AttoparsecParser            String ByteString Regex)) txt ]
  where name = "regex" ++ show id ++ " parser"

txt0 = ".ab."
txt1 = ".ab*(cd*)*((e|f|g+){2,10}.){11}"
txt2 = "((.(a|b*|c|d*|e|f*|g|h+|i|j{3,20}|k|(l(m(n(op*)*)*)*)*)*.){10,20})+"
txt3 = "(.(a|b*|c|d*|e|f*|g|h+|i|j{3,20}|k|(l(m(n(op*)*)*)*)*)*.){10,20}"
txt4 = "(.(a|b*|c|d*|e|f*|g|h+|lei|j{3,20}|k|(lmnop*)*)*.){10,20}"
txt5 = "a*b*c*d*e*f*g*h*i*j*k*l*m*n*o*(p*(q*(r*(s*(t*(u*(v*(w*(x*(y*z*)*)*)*)*)*)*)*)*)*)*"
txt6 = "(((abc){10,20}cde*f*){10,20}((g(h|i|jk*)*)+){10,20}){10,20}"

main = defaultMain [ regexParser 0 txt0
                   , regexParser 1 txt1
                   , regexParser 2 txt2
                   , regexParser 3 txt3
                   , regexParser 4 txt4
                   , regexParser 5 txt5
                   , regexParser 6 txt6 ]
