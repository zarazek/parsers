{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative (Applicative, (<$>), (<*>), (<*), (*>), pure, Alternative, (<|>), empty)
import Control.Monad (MonadPlus, mzero, mplus, guard, foldM)
import Data.List (foldl', minimumBy, maximumBy, find)
import Data.Char (ord)
import Control.Monad.Trans.State.Lazy
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Debug.Trace
import qualified Data.MemoCombinators as Memo

data Regex = Epsilon
           | Any
           | Letter Char
           | Range [Char]
           | Star Regex
           | Plus Regex
           | Concatenation Regex Regex
           | Alternative Regex Regex
           | Repetition Regex Int Int
  deriving (Eq, Show)
  
-- (*>) :: Applicative f => f a -> f b -> f b
-- (<*) :: Applicative f => f a -> f b -> f a

letters = ['A'..'Z'] ++ ['a'..'z']
digits = ['0'..'9']
other = "`~!@#$%^&-_=[];:'\"<>/? "
nonSpecialCharacters = letters ++ digits ++ other
specialCharacters = "|*+{,}().\\"
alphabet = nonSpecialCharacters ++ specialCharacters

regex = union <|> simpleRegex
union = Alternative <$> simpleRegex <* char '|' <*> regex
simpleRegex = concatenation <|> basicRegex
concatenation = Concatenation <$> basicRegex <*> simpleRegex
basicRegex = star <|> plus  <|> exactRepetition <|> rangeRepetition <|> elementaryRegex
star = Star <$> elementaryRegex <* char '*'
plus = Plus <$> elementaryRegex <* char '+'
number = listToNum <$> many1 digit
  where listToNum = foldl' nextDigit 0
        nextDigit num digit = 10*num + digit
exactRepetition = makeRepetition <$> elementaryRegex <* openBrace <*> number <* closeBrace
  where makeRepetition expr count = Repetition expr count count
rangeRepetition = do
  res <- parse
  guard (check res)
  return res
  where parse = Repetition <$> elementaryRegex <* openBrace <*> number <* comma <*> number <* closeBrace
        check (Repetition _ min max) = min < max
openBrace = char '{'
closeBrace = char '}'
comma = char ','
elementaryRegex = group <|> anyR <|> charR
group = char '(' *> regex <* char ')'
anyR = char '.' *> pure Any
charR = ordinaryChar <|> escapedChar
ordinaryChar = Letter <$> oneOf nonSpecialCharacters
escapedChar = Letter <$> (char '\\' *> oneOf specialCharacters)

data StaticInfo = StaticInfo { matchesEmpty :: Bool,
                               starters :: S.Set Char }
  deriving (Show)                               
                                   
sequentialCompose s1 s2 = case s1 of
                            Nothing                     -> Nothing
                            Just (StaticInfo False _  ) -> s1
                            Just (StaticInfo True  st1) -> case s2 of
                                                             Nothing                  -> Nothing
                                                             Just (StaticInfo me st2) -> Just (StaticInfo me (S.union st1 st2))

paralellCompose s1 s2 = do
  StaticInfo matchesEmpty1 starters1 <- s1
  StaticInfo matchesEmpty2 starters2 <- s2
  let newMatchesEmpty = matchesEmpty1 || matchesEmpty2
  let newStarters = S.union starters1 starters2
  return $ StaticInfo newMatchesEmpty newStarters

extractStatic (Parser _ s) = s

data Parser a = Parser (String -> [(a, String)]) (Maybe StaticInfo)

mapFst f (a, b) = (f a, b)
mapSnd f (a, b) = (a, f b)

memo = Memo.list Memo.char

instance Functor Parser where
  fmap f (Parser p s) = Parser combined' s
    where combined' = memo combined
          combined = map (mapFst f) . p
  
useStatic s p str = case s of
                      Nothing                 -> p str
                      Just (StaticInfo me st) -> case str of
                                                   []   | me              -> p str
                                                        | otherwise       -> []
                                                   x:xs | x `S.member` st -> p str
                                                        | otherwise       -> []

instance Applicative Parser where
  pure x = Parser (\str -> [(x, str)]) (Just (StaticInfo True (S.fromList alphabet)))
  (Parser pf sf) <*> ~(Parser pa sa) = Parser combinedDynamic''  combinedStatic
    where combinedDynamic'' = memo combinedDynamic'
          combinedDynamic' = useStatic combinedStatic combinedDynamic
          combinedDynamic = concatMap combine . pf
            where combine (f, str) = map (mapFst f) (pa str)
          combinedStatic = sequentialCompose sf sa

instance Alternative Parser where
  empty = Parser (const []) (Just (StaticInfo False S.empty))
  (Parser pl sl) <|> (Parser pr sr) = Parser p'' s
    where p'' = memo p'
          p' = useStatic s p
          p str = pl str ++ pr str
          s = paralellCompose sl sr

instance Monad Parser where
  return = pure
  (Parser pa sa) >>= f = Parser combinedDynamic combinedStatic
    where combinedDynamic'' = memo combinedDynamic'
          combinedDynamic' = useStatic combinedStatic combinedDynamic
          combinedDynamic = concatMap combine . pa
            where combine (a, str) = pb str
                    where Parser pb _ = f a
          combinedStatic = sequentialCompose sa Nothing

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)


char c = Parser p s
  where p []                 = []
        p (x:xs) | x == c    = [(x, xs)]
                 | otherwise = []
        s = Just (StaticInfo False (S.singleton c))

oneOf lst = Parser p s
  where p [] = []
        p (x:xs) | x `S.member` set = [(x,xs)]
                 | otherwise        = []
        set = S.fromList lst
        s = Just (StaticInfo False set)

digit = charToDigit <$> oneOf ['0'..'9']
  where charToDigit c = ord c - ord '0'
  
epsilon = pure ()

many exp = (:) <$> exp <*> (many exp) <|> epsilon *> pure []
many1 exp = (:) <$> exp <*> many1 exp <|> singleton <$> exp
  where singleton x = [x]

parse (Parser p _) txt = p txt

listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

parseSimple p = fmap fst . listToMaybe . filter (null . snd) . parse p 

-- (*>) :: Applicative f => f a -> f b -> f b
-- (<*) :: Applicative f => f a -> f b -> f a

prettyPrint Epsilon                              = ""
prettyPrint Any                                  = "."
prettyPrint (Letter c)                           = [c]
prettyPrint (Star e)                             = printElementary e ++ "*"
prettyPrint (Plus e)                             = printElementary e ++ "+"
prettyPrint (Concatenation e1 e2)                = printBasic e1 ++ printBasic e2
prettyPrint (Alternative e1 e2)                  = prettyPrint e1 ++ "|" ++ prettyPrint e2
prettyPrint (Repetition e min max) | min == max  = printElementary e ++ "{" ++ show min ++ "}"
                                   | otherwise   = printElementary e ++ "{" ++ show min ++ "," ++ show max ++ "}"
                                   
inParens e = "(" ++ prettyPrint e ++ ")"

printElementary e@Epsilon    = prettyPrint e
printElementary e@Any        = prettyPrint e
printElementary e@(Letter c) = prettyPrint e
printElementary e            = inParens e

printBasic e@(Alternative _ _) = inParens e
printBasic e                   = prettyPrint e

isNullable Epsilon               = True
isNullable Any                   = False
isNullable (Letter _)            = False
isNullable (Star _)              = True
isNullable (Plus e)              = isNullable e
isNullable (Concatenation e1 e2) = isNullable e1 && isNullable e2
isNullable (Alternative e1 e2)   = isNullable e1 || isNullable e2
isNullable (Repetition e min _)  = isNullable e || min == 0

starts Epsilon    = []
starts Any        = alphabet
starts (Letter c) = [c]
starts (Star e)   = starts e
starts (Plus e)   = starts e
starts (Concatenation e1 e2) = starts e1 ++ if isNullable e1 then starts e2 else []
starts (Alternative e1 e2)   = starts e1 ++ starts e2
starts (Repetition e _ max)  = if max == 0 then [] else starts e

ends Epsilon               = []
ends Any                   = alphabet
ends (Letter c)            = [c]
ends (Star e)              = ends e
ends (Plus e)              = ends e
ends (Concatenation e1 e2) = ends e2 ++ if isNullable e2 then ends e1 else []
ends (Alternative e1 e2)   = ends e1 ++ ends e2
ends (Repetition e _ max)   = if max == 0 then [] else ends e

type StateLabel = Int
data EdgeLabel = EpsilonEdge | SymbolEdge Char
  deriving (Eq, Ord, Show)
  
newtype TransitionTable = TransitionTable { unTrans :: M.Map (StateLabel, EdgeLabel) (S.Set StateLabel) }

nextStates (TransitionTable table) state edge = M.findWithDefault S.empty (state, edge) table

connection from to edge = TransitionTable $ M.singleton (from, edge) (S.singleton to)

merge = TransitionTable . foldl' mergeTwo M.empty . map unTrans
  where mergeTwo = M.unionWith S.union

data NFA = NFA { start :: StateLabel,
                 end   :: StateLabel,
                 trans :: TransitionTable }

newLabel = state f
  where f i = (i, i+1)

desugar (Star e) = Star (desugar e)
desugar (Plus e) = Concatenation e' (Star e')
  where e' = desugar e
desugar (Concatenation e1 e2) = Concatenation (desugar e1) (desugar e2)
desugar (Alternative e1 e2) = Alternative (desugar e1) (desugar e2)
desugar (Repetition e min max) = Concatenation (chain min e') (chain (max - min) (Alternative e' Epsilon))
  where chain 0 e = Epsilon
        chain 1 e = e
        chain n e = Concatenation e (chain (n-1) e)
        e' = desugar e
desugar Any = foldl' f Epsilon alphabet
  where f e c = Alternative e (Letter c)
desugar e =  e
  
regexToNFA Epsilon = do
  start <- newLabel
  end <- newLabel
  return $ NFA start end (connection start end EpsilonEdge)
regexToNFA (Letter c) = do
  start <- newLabel
  end <- newLabel
  return $ NFA start end (connection start end (SymbolEdge c))
regexToNFA (Star e) = do
  outerStart <- newLabel
  outerEnd <- newLabel
  (NFA innerStart innerEnd trans) <- regexToNFA e
  let c1 = connection outerStart innerStart EpsilonEdge
  let c2 = connection innerEnd outerEnd EpsilonEdge
  let c3 = connection innerEnd innerStart EpsilonEdge
  let c4 = connection outerStart outerEnd EpsilonEdge
  return $ NFA outerStart outerEnd (merge [c1, c2, c3, c4, trans])
regexToNFA (Concatenation e1 e2) = do
  (NFA start1 end1 trans1) <- regexToNFA e1
  (NFA start2 end2 trans2) <- regexToNFA e2
  let c = connection end1 start2 EpsilonEdge
  return $ NFA start1 end2 (merge [trans1, trans2, c])
regexToNFA (Alternative e1 e2) = do
  outerStart <- newLabel
  outerEnd <- newLabel
  (NFA innerStart1 innerEnd1 trans1) <- regexToNFA e1
  (NFA innerStart2 innerEnd2 trans2) <- regexToNFA e2
  let c1 = connection outerStart innerStart1 EpsilonEdge
  let c2 = connection outerStart innerStart2 EpsilonEdge
  let c3 = connection innerEnd1 outerEnd EpsilonEdge
  let c4 = connection innerEnd2 outerEnd EpsilonEdge
  return $ NFA outerStart outerEnd (merge [c1, c2, c3, c4, trans1, trans2])
  
regexToNFA' regex = evalState (regexToNFA (desugar regex)) 0

eClosureAll table states = fst $ until (S.null . snd) eReachAll (states, states)
  where eReachAll (closure, boundary) = S.foldl eReach (closure, S.empty) boundary
        eReach tuple state = S.foldl addState tuple (nextStates table state EpsilonEdge)
        addState (closure, boundary) state = (newClosure, newBoundary)
          where (newClosure, wasInserted) = insertAndCheck state closure
                newBoundary = if wasInserted then S.insert state boundary else boundary
        insertAndCheck elt oldSet = (newSet, wasInserted)
          where newSet = S.insert elt oldSet
                wasInserted = S.size newSet > S.size oldSet

eClosure table state = eClosureAll table (S.singleton state) 

mapArgs f g h x y = h (f x) (g y)
compareSecond = mapArgs snd snd compare
minWith f xs = fst $ minimumBy compareSecond $ zip xs (map f xs)
maxWith f xs = fst $ maximumBy compareSecond $ zip xs (map f xs)

reachAll table states edge = S.foldl reach S.empty states
  where reach set state = S.union set (nextStates table state edge)

memoSet :: Ord a => Memo.Memo a -> Memo.Memo (S.Set a)  
memoSet memo = Memo.wrap S.fromList S.toList (Memo.list memo)
  
nfaToDFA (NFA start end table) = until (S.null . get3rd) exploreBoundary (M.empty, initialStates, initialStates)  
  where initialStates = S.singleton (eClosure table start)
        exploreBoundary (dfa, states, boundary) = trace ("states " ++ show (S.size states) ++ ", boundary " ++ show (S.size boundary)) $ S.foldl exploreState (dfa, states, S.empty) boundary
        exploreState tuple state = foldl' (exploreEdge state) tuple alphabet
        exploreEdge state tuple@(dfa, states, boundary) edge = if S.null newState then tuple else (newDfa, newStates, newBoundary)
          where newState = eClosureAll' (reachAll' state edge)
                newDfa = M.insertWith checkEq (state, edge) newState dfa
                (newStates, wasInserted) = insertAndCheck newState states
                newBoundary = if wasInserted then S.insert newState boundary else boundary
                checkEq old new = if old == new then old else error "dupa"
        -- eClosureAll' = (memoSet Memo.integral) (eClosureAll table)
        eClosureAll' = eClosureAll table
        reachAll' state edge = reachAll table state (SymbolEdge edge)
        insertAndCheck elt set = (newSet, wasInserted)
          where newSet = S.insert elt set
                wasInserted = S.size newSet > S.size set

impossibleConflict str _ _ = error str

bidiMapEmpty = (M.empty, M.empty)

bidiMapSize (m1, _) = M.size m1

bidiMapInsert l1 l2 (m1, m2) = (newM1, newM2)
  where newM1 = M.insertWith (impossibleConflict "dupa1") l1 l2 m1
        newM2 = M.insertWith (impossibleConflict "dupa2") l2 l1 m2

forwardLookup l1 (m1, _) = M.lookup l1 m1

reverseLookup l2 (_, m2) = M.lookup l2 m2

foldSetM f a s = foldM f a (S.toList s)

addConnection from to edge dfa = M.insertWith (impossibleConflict "dupa3") (from, edge) to dfa

iterateUntilM pred f x | pred x    = return x
                       | otherwise = f x >>= iterateUntilM pred f

nfaToDfaSt (NFA start end table) = do
  initialLabel <- newLabel
  let initialStates = bidiMapInsert (eClosure table start) initialLabel bidiMapEmpty
  let initialBoundary = S.singleton initialLabel
  iterateUntilM (S.null . get3rd) exploreBoundary (M.empty, initialStates, initialBoundary)
    where exploreBoundary (dfa, states, boundary) = trace ("states " ++ show (bidiMapSize states) ++ ", boundary " ++ show (S.size boundary)) $ foldSetM exploreState (dfa, states, S.empty) boundary
          exploreState tuple state = foldM (exploreEdge state) tuple alphabet
          exploreEdge state tuple@(dfa, states, boundary) edge = do
            let Just stateSet = reverseLookup state states
            let newStateSet = eClosureAll' (reachAll table stateSet (SymbolEdge edge))
            if S.null newStateSet then
              return tuple
            else do
              case forwardLookup newStateSet states of
                Just label -> return (addConnection state label edge dfa, states, boundary)
                Nothing -> do
                             label <- newLabel
                             let newDfa = addConnection state label edge dfa
                             let newStates = bidiMapInsert newStateSet label states
                             let newBoundary = S.insert label boundary
                             return (newDfa, newStates, newBoundary)
          eClosureAll' = (memoSet Memo.integral) (eClosureAll table)
          -- reachAll' = Memo.memo2 (memoSet Memo.integral) Memo.char f
            -- where f stateSet c = reachAll table stateSet (SymbolEdge c)


nfaToDfa' nfa = evalState (nfaToDfaSt nfa) 0

class Has1st s t a b | s -> a, s b -> t where
  get1st :: s -> a
  set1st :: b -> s -> t

instance Has1st (a, b) (z, b) a z where
  get1st (a, _) = a
  set1st a (_, b) = (a, b)

instance Has1st (a, b, c) (z, b, c) a z where
  get1st (a, _, _) = a
  set1st a (_, b, c) = (a, b, c)

class Has2nd s t a b | s -> a, s b -> t where
  get2nd :: s -> a
  set2nd :: b -> s -> t
  
instance Has2nd (a, b) (a, z) b z where
  get2nd (_, b) = b
  set2nd b (a, _) = (a, b)

instance Has2nd (a, b, c) (a, z, c) b z where
  get2nd (_, b, _) = b
  set2nd b (a, _, c) = (a, b, c)
  
class Has3rd s t a b | s -> a, s b -> t where
  get3rd :: s -> a
  set3rd :: b -> s -> t
  
instance Has3rd (a, b, c) (a, b, z) c z where
  get3rd (_, _, c) = c
  set3rd c (a, b, _) = (a, b, c)
  
------------------------------------

txt1 = ".ab*(cd*)*((e|f|g+){2,10}.){11}"
txt2 = "((.(a|b*|c|d*|e|f*|g|h+|i|j{3,20}|k|(l(m(n(op*)*)*)*)*)*.){10,20})+"
txt3 = "(.(a|b*|c|d*|e|f*|g|h+|i|j{3,20}|k|(l(m(n(op*)*)*)*)*)*.){10,20}"
txt4 = "(.(a|b*|c|d*|e|f*|g|h+|lei|j{3,20}|k|(lmnop*)*)*.){10,20}"
txt5 = "a*b*c*d*e*f*g*h*i*j*k*l*m*n*o*(p*(q*(r*(s*(t*(u*(v*(w*(x*(y*z*)*)*)*)*)*)*)*)*)*)*"
txt6 = "(((abc){10,20}cde*f*){10,20}((g(h|i|jk*)*)+){10,20}){10,20}"

maxState nfa = maxWith (S.size . eClosure (trans nfa)) [start nfa .. end nfa]

fromJust (Just x) = x

fromRight (Right x) = x

-- newtype GeneratedString = GeneratedString String
  -- deriving (Show, Eq, Ord)

-- instance Monad m => Serial m GeneratedString where
  -- series = generate (\n -> concatMap (map GeneratedString . go) [0..n])
    -- where go 0 = [""]
          -- go n = [x:xs | x <- alphabet, xs <- go (n-1)]

-- grammarIsUnambiguous (GeneratedString str) = not (null result) ==> length result == 1
  -- where result = filter (null . snd) $ parse regex str

-- uniq [] = []
-- uniq (x:xs) = go xs
  -- where go []                     = [x]
        -- go lst@(y:ys) | x == y    = go ys
                      -- | otherwise = x : uniq lst
                      
process txt = putStrLn (txt ++ " " ++ (show $ M.size $ get1st $ nfaToDfa' $ regexToNFA' $ fromJust $ parseSimple regex txt))
                      
main = do
  process txt1
  process txt2
  process txt3
  process txt4
  process txt5
  process txt6

-- data DeclSpecs
-- data Declarator
-- data DeclList
-- data CompoundStat
-- data InitDeclaratorList
-- data Declaration = FunctionDefinition (Maybe DeclSpecs) Declarator (Maybe DeclList) CompoundStat
--                  | Declaration DeclSpecs (Maybe InitDeclaratorList) 

-- translationUnit = many1 externalDecl
-- externalDecl = functionDefinition <|> decl
-- functionDefinition = FunctionDefinition <$> (Just <$> declSpecs) <*> declarator <*> (Just <$> declList) <*> compoundStat <|>
--                      FunctionDefinition <$> pure Nothing         <*> declarator <*> (Just <$> declList) <*> compoundStat <|>
--                      FunctionDefinition <$> (Just <$> declSpecs) <*> declarator <*> pure Nothing        <*> compoundStat <|>
--                      FunctionDefinition <$> pure Nothing         <*> declarator <*> pure Nothing        <*> compoundStat

-- decl = Declaration <$> declSpecs <*> (Just <$> initDeclaratorList) <* semicolon <|>
--        Declaration <$> declSpecs <*> pure Nothing                  <* semicolon

-- declList = many1 decl

-- declSpecs = undefined
-- declarator = undefined
-- compoundStat = undefined
-- initDeclaratorList = undefined

-- semicolon = char ';'


