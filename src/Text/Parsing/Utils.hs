module Text.Parsing.Utils (mapFst, mapSnd, replaceFst, replaceSnd,
                           recurParseL, recurParseM, recurParseG,
                           recurParseML, recurParseMM, recurParseMG) where

mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
replaceFst x (_, y) = (x, y)
replaceSnd y (x, _) = (x, y)

recurParseL :: (lst -> [(a, lst)]) -> [a] -> lst -> [([a], lst)]
recurParseL p as str = map (mapFst reverse) (go as str)
  where go soFar str = concatMap f (p str) ++ [(soFar, str)]
                         where f (x, str') = go (x:soFar) str'

recurParseM :: (lst -> Maybe (a, lst)) -> [a] -> lst -> Maybe ([a], lst)
recurParseM p as str = mapFst reverse <$> go as str
  where go soFar str = (p str >>= f) <|>  Just (soFar, str)

recurParseG :: (Monad m, Alternative m) => (lst -> m (a, lst)) -> [a] -> lst -> m ([a], lst)
recurParseG p as str = mapFst reverse <$> go as str
  where go soFar str = (p str >>= f) <|> (pure (soFar, str))
          where f (x, str') = go (x:soFar) str'

combineL :: Monad m => (a -> m [b]) -> [a] -> m [b]
combineL f mx = concat <$> mapM f mx
 
recurParseML :: Monad m => (lst -> m [(a, lst)]) -> [a] -> lst -> m [([a], lst)]
recurParseML p as str = map (mapFst reverse) <$> go as str
  where go soFar str = do
                         mx <- p str
                         let f (x, str') = go (x:soFar) str'
                         init <- combineL f mx 
                         return (init ++ [(soFar, str)])

combineM :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
combineM f mx = case mx of
                  Just x -> f x
                  Nothing -> return Nothing

recurParseMM :: Monad m => (lst -> m (Maybe (a, lst))) -> [a] -> lst -> m (Maybe ([a], lst))
recurParseMM p as str = fmap (mapFst reverse) <$> go as str
  where go soFar str = do
                         mx <- p str
                         let f (x, str') = go (x:soFar) str'
                         init <- combineM f mx
                         return (init <|> pure (soFar, str))

combineG :: (Monad m, Monad t, Traversable t) => (a -> m (t b)) -> t a -> m (t b)
combineG f mx = join <$> mapM f mx

recurParseMG :: (Monad m, Monad t, Traversable t, Alternative t) => (lst -> m (t (a, lst))) -> t a -> m (t ([a], lst)) 
recurParseMG p as str = fmap (mapFst reverse) <$> go as str
  where go soFar str = do
                         mx <- p str
                         let f (x, str') = go (x:soFar) str'
                         init <- combineG f mx
                         return (init <|> pure (soFar, str))
