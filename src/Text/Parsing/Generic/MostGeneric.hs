{-# LANGUAGE GADTs, TypeFamilies, RankNTypes, ScopedTypeVariables, RecursiveDo #-}

module Text.Parsing.Generic.MostGeneric (Parser, any, satisfy, char, oneOf, decimal, string, label,
                                         parseST, simpleParseST) where 

import Prelude hiding (null, any)
import Control.Monad (MonadPlus, join, mfilter)
import Control.Monad.Fix (MonadFix)
import Control.Monad.ST (ST, runST)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')
import Control.Monad.Trans.State.Strict (StateT, get, modify, evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Applicative (Alternative(..))
import Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as HM
import qualified Data.FastSet as FS
import qualified Data.MapLike as ML
import Data.ListLike
import Data.Opaque
import Data.Char (ord)
import Data.List (foldl')

data Parser elt lbl lst a where
  Fail         :: Parser elt lbl lst a
  Empty        :: a -> Parser elt lbl lst a
  Any          :: Parser elt lbl lst elt
  Satisfy      :: (elt -> Bool) -> Maybe (FS.FastSet elt) -> Parser elt lbl lst elt
  Str          :: lst -> Parser elt lbl lst lst
  Fmap         :: (a -> b) -> Parser elt lbl lst a -> Parser elt lbl lst b
  Ap           :: Parser elt lbl lst (a -> b) -> Parser elt lbl lst a -> Parser elt lbl lst b
  DiscardRight :: Parser elt lbl lst a -> Parser elt lbl lst b -> Parser elt lbl lst a
  DiscardLeft  :: Parser elt lbl lst a -> Parser elt lbl lst b -> Parser elt lbl lst b
  Or           :: Parser elt lbl lst a -> Parser elt lbl lst a -> Parser elt lbl lst a
  Many         :: Parser elt lbl lst a -> Parser elt lbl lst [a]
  Some         :: Parser elt lbl lst a -> Parser elt lbl lst [a]
  Bind         :: Parser elt lbl lst a -> (a -> Parser elt lbl lst b) -> Parser elt lbl lst b
  Label        :: lbl -> Parser elt lbl lst a -> Parser elt lbl lst a

instance Functor (Parser elt lbl lst) where
  fmap = Fmap

instance Applicative (Parser elt lbl lst) where
  pure = Empty
  (<*>) = Ap
  (<*) = DiscardRight
  (*>) = DiscardLeft

instance Alternative (Parser elt lbl lst) where
  empty = Fail
  (<|>) = Or
  many = Many
  some = Some

instance Monad (Parser elt lbl lst) where
  (>>=) = Bind

instance MonadPlus (Parser elt lbl lst)

any :: Parser elt lst lbl elt
any = Any

satisfy :: (elt -> Bool) -> Parser elt lbl lst elt
satisfy pred = Satisfy pred Nothing

satisfyWithSet :: (elt -> Bool) -> FS.FastSet elt -> Parser elt lbl lst elt
satisfyWithSet pred set = Satisfy pred (Just set)

char :: (Eq elt, FS.HasFastSet elt) => elt -> Parser elt lbl lst elt
char c = satisfyWithSet (== c) (FS.fromList [c])

oneOf :: FS.HasFastSet elt => [elt] -> Parser elt lbl lst elt
oneOf lst = satisfyWithSet pred set
  where pred c = c `FS.member` set
        set = FS.fromList lst

digit :: Parser Char lbl lst Int
digit = charToDigit <$> oneOf ['0'..'9']
  where charToDigit c = ord c - ord '0'

decimal :: Parser Char lbl lst Integer
decimal = listToNum <$> some digit
  where listToNum = foldl' nextDigit 0
        nextDigit num digit = 10*num + toInteger digit

string = Str

sequenceP :: (Eq elt, FS.HasFastSet elt, Traversable t) => t elt -> Parser elt lbl lst (t elt)
sequenceP seq = (sequenceA . fmap char) seq

label :: lbl -> Parser elt lbl lst b -> Parser elt lbl lst b
label = Label

class MonadFix m => MonadRef m where
  type Ref m :: * -> *
  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()
  modifyRef :: Ref m a -> (a -> a) -> m ()

instance MonadRef IO where
  type Ref IO = IORef
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef
  modifyRef = modifyIORef'

instance MonadRef (ST s) where
  type Ref (ST s) = STRef s
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef
  modifyRef = modifySTRef'

instance MonadRef m => MonadRef (StateT s m) where
  type Ref (StateT s m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref val = lift (writeRef ref val)
  modifyRef ref f = lift (modifyRef ref f)

type TranslatorState lbl = HM.HashMap lbl Opaque
type TranslatorMonadT m lbl = StateT (TranslatorState lbl) m

mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
replaceFst x (_, y) = (x, y)
replaceSnd y (x, _) = (x, y)

combineG :: (Applicative m, Monad t, Traversable t) => (a -> m (t b)) -> t a -> m (t b)
combineG f mx = (>>= id) <$> traverse f mx

recurParseMG :: (Monad m, Monad t, Alternative t, Traversable t) => (lst -> m (t (a, lst))) -> [a] -> lst -> m (t ([a], lst)) 
recurParseMG p as lst = fmap (mapFst reverse) <$> go as lst
  where go soFar str = do
                         xs <- p str
                         let f (x, str') = go (x:soFar) str'
                         init <- combineG f xs
                         return (init <|> pure (soFar, str))
 
translateMemo :: forall mref map lbl lst elt mres a. (MonadRef mref,
                                                      ML.MapLike map lst,
                                                      Eq lbl, Hashable lbl,
                                                      ListLike lst elt,
                                                      Monad mres, Traversable mres, Alternative mres)  =>
                                                     (forall v. map v) ->
                                                     Parser elt lbl lst a ->
                                                     TranslatorMonadT mref lbl (lst -> TranslatorMonadT mref lbl (mres (a, lst)))
translateMemo emptyCache parser = go parser
  where go :: forall b. Parser elt lbl lst b -> TranslatorMonadT mref lbl (lst -> TranslatorMonadT mref lbl (mres (b, lst)))
        go parser = case parser of
                      Fail -> return (const (return empty))
                      Empty a -> return p
                                   where p lst = return (pure (a, lst))
                      Any -> return p
                               where p = uncons onEmpty onCons
                                     onEmpty = return empty
                                     onCons hd tl = return (pure (hd, tl))
                      Satisfy f _ -> return p
                                       where p = uncons onEmpty onCons
                                             onEmpty = return empty
                                             onCons hd tl = return $! case f hd of
                                                                        False -> empty
                                                                        True  -> pure (hd, tl)
                      Str str -> return p
                                   where p = dropPrefix str onFailure onSuccess
                                         onFailure = return empty
                                         onSuccess rest = return (pure (str, rest))
                      Fmap f parser' -> do
                                          p <- go parser'
                                          let p' = fmap (fmap (mapFst f)) . p
                                          return p'
                      Ap parserF parserA -> do
                                              pf <- go parserF
                                              pa <- go parserA
                                              let g (f, lst) = fmap (mapFst f) <$> pa lst
                                              let p lst = pf lst >>= combineG g
                                              memo p
                      DiscardRight parserA parserB -> do
                                                        pa <- go parserA
                                                        pb <- go parserB
                                                        let f (a, lst) = fmap (replaceFst a) <$> pb lst
                                                        let p lst = pa lst >>= combineG f
                                                        memo p
                      DiscardLeft parserA parserB -> do
                                                       pa <- go parserA
                                                       pb <- go parserB
                                                       let p lst = pa lst >>= combineG (pb . snd)
                                                       memo p
                      Or parserL parserR -> do
                                              pl <- go parserL
                                              pr <- go parserR
                                              let p lst = (<|>) <$> pl lst <*> pr lst
                                              memo p
                      Many parser' -> do
                                        p <- go parser'
                                        memo (recurParseMG p [])
                      Some parser' -> do
                                        p <- go parser'
                                        let f (x, lst) = recurParseMG p [x] lst
                                        let p' lst = p lst >>= combineG f
                                        memo p'   
                      Bind parserA f -> do
                                          pa <- go parserA
                                          let g (a, lst) = do
                                                             pb <- go (f a)
                                                             pb lst
                                          let p str = pa str >>= combineG g
                                          memo p
                      Label label parser' -> do
                                               translationCache <- get
                                               case HM.lookup label translationCache of
                                                 Just op -> return (fromOpaque op)
                                                 Nothing  -> mdo
                                                               modify (HM.insert label (toOpaque p))
                                                               p <- go parser'
                                                               return p
        memo :: forall b. (lst -> TranslatorMonadT mref lbl (mres (b, lst))) -> TranslatorMonadT mref lbl (lst -> TranslatorMonadT mref lbl (mres (b, lst)))
        memo p = do
                   cacheRef <- newRef emptyCache
                   return $ \lst -> do
                                      c <- readRef cacheRef
                                      case ML.lookup lst c of
                                        Just res -> return res
                                        Nothing -> do
                                                     res <- p lst
                                                     modifyRef cacheRef (ML.insert lst res)
                                                     return res                          

parseST :: (ML.MapLike map lst, Eq lbl, Hashable lbl, ListLike lst elt, MonadPlus mres, Traversable mres) => (forall v. map v) -> Parser elt lbl lst a -> lst -> mres (a, lst)
parseST emptyCache parser lst = runST $ flip evalStateT HM.empty $ do
                                                                      p <- translateMemo emptyCache parser
                                                                      p lst

simpleParseST ::(ML.MapLike map lst, Eq lbl, Hashable lbl, ListLike lst elt, MonadPlus mres, Traversable mres) => (forall v. map v) -> Parser elt lbl lst a -> lst -> mres a
simpleParseST emptyCache parser = fmap fst . mfilter (null . snd) . parseST emptyCache parser
