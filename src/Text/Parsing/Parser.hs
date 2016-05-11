{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving #-}

module Text.Parsing.Parser (Parser(..), AttoparsecParser) where

import qualified Text.Parsing.List.Simple as LS
import qualified Text.Parsing.List.StaticInfo as LI
import qualified Text.Parsing.List.Memo as LM
import qualified Text.Parsing.List.Combined as LC
import qualified Text.Parsing.List.NewMemo as LN
import qualified Text.Parsing.Maybe.Simple as MS
import qualified Text.Parsing.Maybe.StaticInfo as MI
import qualified Text.Parsing.Generic.Simple as GS
import qualified Text.Parsing.Generic.StaticInfo as GI
import qualified Text.Parsing.Generic.MostGeneric as MMG
import qualified Data.Attoparsec.Internal.Types as AI (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Attoparsec.Text as AT
import Data.ListLike
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.Maybe (listToMaybe)
import qualified Data.CharSet as CS (member, fromList)
import qualified Data.ListTrie.Base.Map as M (WrappedIntMap)
import qualified Data.ListTrie.Patricia.Map as ST (TrieMap, empty)
import qualified Data.Trie as BT (empty)
import Data.Hashable (Hashable)

class MonadPlus (p lbl lst) => Parser p lbl lst where
  char :: Char -> p lbl lst Char
  satisfy :: (Char -> Bool) -> p lbl lst Char
  oneOf :: [Char] -> p lbl lst Char
  decimal :: p lbl lst Integer
  string :: lst -> p lbl lst lst
  label :: lbl -> p lbl lst a -> p lbl lst a
  label _ p = p
  simpleParse :: p lbl lst a -> lst -> Maybe a

instance ListLike lst Char => Parser LS.Parser lbl lst where
  char = LS.char
  satisfy = LS.satisfy
  oneOf = LS.oneOf
  decimal = LS.decimal
  string = LS.string
  simpleParse p lst = listToMaybe $ LS.simpleParse p lst

instance ListLike lst Char => Parser (LI.Parser Char) lbl lst where
  char = LI.char
  satisfy = LI.satisfy
  oneOf = LI.oneOf
  decimal = LI.decimal
  string = LI.string
  simpleParse p lst = listToMaybe $ LI.simpleParse p lst

instance (ListLike lst Char) => Parser LM.Parser lbl lst where
  char = LM.char
  satisfy = LM.satisfy
  oneOf = LM.oneOf
  decimal = LM.decimal
  string = LM.string
  simpleParse p lst = listToMaybe $ LM.simpleParse p lst

instance (ListLike lst Char) => Parser (LC.Parser Char) lbl lst where
  char = LC.char
  satisfy = LC.satisfy
  oneOf = LC.oneOf
  decimal = LC.decimal
  string = LC.string
  simpleParse p lst = listToMaybe $ LC.simpleParse p lst

instance (Eq lbl, Hashable lbl, Show lbl) => Parser (LN.Parser Char) lbl String where
  char = LN.char
  satisfy = LN.satisfy
  oneOf = LN.oneOf
  decimal = LN.decimal
  string = LN.string
  label = LN.label
  simpleParse p lst = listToMaybe $ LN.simpleParseST (ST.empty :: ST.TrieMap M.WrappedIntMap Char v) p lst

instance (Eq lbl, Hashable lbl, Show lbl) => Parser (LN.Parser Char) lbl ByteString where
  char = LN.char
  satisfy = LN.satisfy
  oneOf = LN.oneOf
  decimal = LN.decimal
  string = LN.string
  label = LN.label
  simpleParse p lst = listToMaybe $ LN.simpleParseST BT.empty p lst

instance ListLike lst Char => Parser MS.Parser lbl lst where
  char = MS.char
  satisfy = MS.satisfy
  oneOf = MS.oneOf
  decimal = MS.decimal
  string = MS.string
  simpleParse = MS.simpleParse

instance ListLike lst Char => Parser (MI.Parser Char) lbl lst where
  char = MI.char
  satisfy = MI.satisfy
  oneOf = MI.oneOf
  decimal = MI.decimal
  string = MI.string
  simpleParse = MI.simpleParse

instance ListLike lst Char => Parser (GS.Parser []) lbl lst where
  char = GS.char
  satisfy = GS.satisfy
  oneOf = GS.oneOf
  decimal = GS.decimal
  string = GS.string
  simpleParse p lst = listToMaybe $ GS.simpleParse p lst

instance ListLike lst Char => Parser (GS.Parser Maybe) lbl lst where
  char = GS.char
  satisfy = GS.satisfy
  oneOf = GS.oneOf
  decimal = GS.decimal
  string = GS.string
  simpleParse = GS.simpleParse

instance ListLike lst Char => Parser (GI.Parser [] Char) lbl lst where
  char = GI.char
  satisfy = GI.satisfy
  oneOf = GI.oneOf
  decimal = GI.decimal
  string = GI.string
  simpleParse p lst = listToMaybe $ GI.simpleParse p lst

instance ListLike lst Char => Parser (GI.Parser Maybe Char) lbl lst where
  char = GI.char
  satisfy = GI.satisfy
  oneOf = GI.oneOf
  decimal = GI.decimal
  string = GI.string
  simpleParse = GI.simpleParse

instance (Hashable lbl, Eq lbl) => Parser (MMG.Parser Char) lbl String where
  char = MMG.char
  satisfy = MMG.satisfy
  oneOf = MMG.oneOf
  decimal = MMG.decimal
  string = MMG.string
  label = MMG.label
  simpleParse = MMG.simpleParseST (ST.empty :: ST.TrieMap M.WrappedIntMap Char v)


newtype AttoparsecParser lbl lst a = AttoparsecParser (AI.Parser lst a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance Parser AttoparsecParser lbl ByteString where
  char = AttoparsecParser . AB.char
  satisfy = AttoparsecParser . AB.satisfy
  oneOf lst = AttoparsecParser (AB.satisfy pred)
    where pred c = c `CS.member` set
          set = CS.fromList lst
  decimal = AttoparsecParser AB.decimal
  string = AttoparsecParser . AB.string
  simpleParse (AttoparsecParser p) lst = either (const Nothing) Just $ AB.parseOnly p lst

instance Parser AttoparsecParser lbl Text where
  char = AttoparsecParser . AT.char
  satisfy = AttoparsecParser . AT.satisfy
  oneOf lst = AttoparsecParser (AT.satisfy pred)
    where pred c = c `CS.member` set
          set = CS.fromList lst
  decimal = AttoparsecParser AT.decimal
  string = AttoparsecParser . AT.string
  simpleParse (AttoparsecParser p) lst = either (const Nothing) Just $ AT.parseOnly p lst
