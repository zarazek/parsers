{-# LANGUAGE BangPatterns #-}

import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import qualified Data.IntMap.Strict as IM
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import Control.Monad.State.Strict (State, get, modify, evalState)
import Debug.Trace (trace)

traceBlocks :: Show a => String -> IM.IntMap a -> b -> b
traceBlocks str blocks = trace (str ++ " " ++ show (IM.toList blocks))

bool :: a -> a -> Bool -> a
bool onFalse onTrue b = case b of
                          False -> onFalse
                          True  -> onTrue

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM act lst = go lst
  where go lst = case lst of
                   []   -> return True
                   x:xs -> act x >>= bool (return False) (go xs)

instance Hashable a => Hashable (IM.IntMap a) where
  hashWithSalt salt = hashWithSalt salt . IM.toList

canWinM :: IM.IntMap Int -> State (M.HashMap (IM.IntMap Int) Bool) Bool
canWinM blocks = case IM.null blocks of
                   True  -> return False
                   False -> do
                              cache <- get
                              case M.lookup blocks cache of
                                Just ans -> return ans
                                Nothing -> do
                                             ans <- not <$> allM canWinM (possibleMoves blocks)
                                             modify (M.insert blocks ans)
                                             return ans


-- canWin :: IM.IntMap Int -> Bool
-- canWin blocks = evalState (canWinM blocks) M.empty

possibleMoves :: IM.IntMap Int -> [IM.IntMap Int]
possibleMoves blocks = [ addBlocks newBlocks rest |
                         ((blockSize, blockCount), rest) <- mapChoices blocks,
                         newBlocks <- movesForBlockSet blockCount blockSize ]
  where addBlocks newBlocksLst oldBlocksMap = foldl' addBlock oldBlocksMap newBlocksLst
        addBlock oldBlocksMap (blockSize, blockCount) = IM.insertWith (+) blockSize blockCount oldBlocksMap

mapChoices :: IM.IntMap a -> [((Int, a), IM.IntMap a)]
mapChoices m = map lookupAndRemove keys
  where lookupAndRemove k = ((k, m IM.! k), IM.delete k m)
        keys = IM.keys m

-- choices :: [a] -> [(a, [a])]
-- choices lst = zipWith choose [0..length lst - 1] (repeat lst)

-- choose :: Int -> [a] -> (a, [a])
-- choose n lst = (x, prefix ++ suffix)
--   where (prefix, suffixAndX) = splitAt n lst
--         x : suffix = suffixAndX

movesForBlockSet :: Int -> Int -> [[(Int, Int)]]
movesForBlockSet cnt size = case cnt of
                              1 -> blockDecompositions 
                              _ -> map ((size, cnt-1):) blockDecompositions
  where blockDecompositions = map (map addCnt) (movesForBlock size)
        addCnt size = (size, 1)

movesForBlock :: Int -> [[Int]]
movesForBlock n = case n of
                   1 -> [[]]
                   2 -> [[1], []]
                   _ -> movesEliminating1 n ++ movesEliminating2 n
  where movesEliminating1  n = [[n-1]] ++ map f [1..(n-1) `div` 2]
          where f k = [k, n-k-1]
        movesEliminating2 n = [[n-2]] ++ map f [1..(n-2) `div` 2]
          where f k = [k, n-k-2]

runLength :: Eq a => [a] -> [(Int, a)]
runLength [] = []
runLength (x:xs) = go 1 xs
  where go !n []                     = [(n, x)]
        go !n lst@(y:ys) | x == y    = go (n+1) ys
                         | otherwise = (n, x) : runLength lst

-- expand :: IM.IntMap Int -> [Int]
-- expand = concatMap (\(x, n) -> replicate n x)  . IM.toList

histogram :: [Int] -> IM.IntMap Int
histogram = foldl' f IM.empty
  where f m x = IM.insertWith (+) x 1 m

blockify :: String -> IM.IntMap Int
blockify = histogram . map fst . filter ((== 'I') . snd) . runLength

readExample = do
  _ <- getLine
  getLine

main = do
  numOfExamples <- read <$> getLine
  examples <- replicateM numOfExamples readExample
  let answers = evalState (mapM canWinM (map blockify examples)) M.empty
  mapM (bool (putStrLn "LOSE") (putStrLn "WIN")) answers
