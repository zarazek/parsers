module Text.Parsing.StaticInfo (StaticInfo(..), sequentialCompose, paralellCompose, useStatic) where

import Control.Applicative (Alternative(..))
import qualified Data.FastSet as FS
import Data.ListLike

data StaticInfo elt = StaticInfo { matchesEmpty :: Bool,
                                   starters :: FS.FastSet elt }

sequentialCompose :: (Monad m, FS.HasFastSet elt) => m (StaticInfo elt) -> m (StaticInfo elt) -> m (StaticInfo elt)
sequentialCompose si1 si2 = do 
  StaticInfo me1 st1 <- si1
  case me1 of
    False -> si1
    True  -> do
               StaticInfo me2 st2 <- si2
               return (StaticInfo me2 (FS.union st1 st2)) 

paralellCompose :: (Monad m, FS.HasFastSet elt) => m (StaticInfo elt) -> m (StaticInfo elt) -> m (StaticInfo elt)
paralellCompose s1 s2 = do
  StaticInfo matchesEmpty1 starters1 <- s1
  StaticInfo matchesEmpty2 starters2 <- s2
  let newMatchesEmpty = matchesEmpty1 || matchesEmpty2
  let newStarters = FS.union starters1 starters2
  return $ StaticInfo newMatchesEmpty newStarters

whenA :: Alternative f => Bool -> f a -> f a
whenA b act = case b of
                False -> empty
                True  -> act

useStatic :: (Alternative f, FS.HasFastSet elt, ListLike lst elt) => Maybe (StaticInfo elt) -> (lst -> f a) -> lst -> f a
useStatic s p = case s of
                 Nothing                 -> p
                 Just (StaticInfo me st) -> p'
                                              where p' str = uncons onEmpty onCons str
                                                      where onEmpty = whenA me res
                                                            onCons c _ = whenA (c `FS.member` st) res
                                                            res = p str
