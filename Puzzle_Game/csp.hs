module Csp where
import Control.Monad  
import Control.Applicative 

-- |generalized types and functions in CSP problems
type Puzzle a b = [(b, a)]
type Constraint   a = (a -> Bool)
type Constraint2  a b = (a -> b -> Bool)
type Csp  a   = [Constraint a]
type Csp2 a b = [Constraint2 a b] 

-- |map one parameter to a couple of constraints
sinmap :: Functor t => a -> t (a -> b) -> t b
sinmap a   = fmap (\f -> f a)

-- |map two parameter to a couple of constraints
binmap :: Functor t => a -> b -> t (a -> b -> c) -> t c
binmap a b = fmap (\f -> f a b)

-- |comfun function: help to refactor other functions like position, onerow, oneblock which returns  Constraint2
-- |onerow, oneblock is in puzzle.hs; position function is in this .hs file
comfun :: (Monad m, Alternative m) =>  m (b, a) -> (c -> m a -> t) -> (d -> b -> Bool) -> c -> d -> t
comfun puzzle f g = \x p -> f x $ do {(ps,val) <- puzzle; guard ( g p ps); return val}

-- |positon constraint: creat env that has input a and b, and return bool
position :: (Eq a, Eq b) => Puzzle a b -> Constraint2 a b
position puzzle  = comfun puzzle (elem) (==)


-- |takes a candidate list [a] and another b as input, by using all (Csp2 a b) constraints to filter invalid value a  
candvalue :: Csp2 a b -> [a] -> b -> [a]
candvalue cs is p = do {i <- is; guard $ notElem False $ binmap i p cs; return $ i}    

-- |similar to candvalue function, but now for (Csp a) constraints c 
candvalue1 :: Csp a  -> [a] -> [a]
candvalue1 cs is = do {i <- is; guard $ notElem False $ sinmap i cs; return $ i}    



-- |replace the value in a list througn positon, 
-- |e.g. let ys = [((1,1),Just 1),((1,2),Just 2)], then repval (Just 8) (1,1) ys = [((1,1),Just 8),((1,2),Just 2)]
repval :: (Eq b, Monad m) => a -> b -> m (b, a) -> m (b, a)
repval val p ys = do 
              (p1,value) <- ys
              if p1 == p 
              then return (p1, val) 
              else return (p1,value)

-- |eg:: let ys = [((1,1),Just 1),((1,2),Just 2)], then replist [Just 7,Just 8] (1,1) ys 
-- |ys= [[((1,1),Just 7),((1,2),Just 2)],[((1,1),Just 8),((1,2),Just 2)]]
replist :: (Eq b, Monad t, Monad m) => t a -> b -> m (b, a) -> t (m (b, a))
replist list p ys = do
              val <- list
              return (repval val p ys)

-- |eg:: let ys = [((1,1),Just 1),((1,2),Just 2)], then listsu ys = [Just 1, Just 2]
listsu :: Monad m => m (b, a) -> m a
listsu su = do
            (p,val) <- su
            return val

-- |solution of (puzzle a b)
solu :: (Eq a, Eq b) => a -> [a] -> [(Puzzle a b) -> Constraint2 a b] -> Puzzle a b -> [Puzzle a b]
solu v vlist  csp2list puz = let x = do {(p,val) <- puz; guard (position puz v p);return p } in case x of 
                                []     ->  [puz]
                                (p:_)  ->  concatMap (solu v vlist  csp2list) (replist list p puz) 
                                               where list = candvalue (sinmap puz csp2list) vlist p 

---- |to check whether Puzzle has solution or not : True -> No solution ; False -> has solution
hasSolution :: (Eq a, Eq b) => a -> [a] -> [(Puzzle a b) -> Constraint2 a b] -> Puzzle a b -> Bool
hasSolution v vlist  csp2list puz = (== []) $ solu v vlist  csp2list puz 

