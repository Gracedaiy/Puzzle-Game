module Animal where
import Csp  
import Data.Char
import Control.Monad  
import Control.Applicative 


-- |------------------------------------------------------------------------------------------------------
-- |Following is an example for  Favorite animal Logic Puzzle 
-- | : http://www.enchantedlearning.com/math/logic/puzzles/favanimal.shtml
type Nat      = Int 
type Pos      = (Nat,Nat)
type Animal a = Puzzle (Maybe a) Pos

-- | creat an Animal game, and initiate the fixed place to be Nothing;
-- |In the output, (Just 1) means the favorite animal for the corresponding person; Otherwise, fixed a value (Just 0)
animalPuzzle :: Animal Int
animalPuzzle = do {i <- [1..4]; j <- [1..4]; return ((i,j),Nothing) }

-- |In every row, Just 1 only appear once:
-- |let ys= [((1,1),Just 0),((1,2),Just 0),((1,3), Just 0),((1,4),Nothing)], then onerow ys (Just 1) (1,4) will be True 
-- |and, then onerow ys (Just 0) (1,4) will be False  
rowcsp :: (Eq a, Num a) => Animal a -> Constraint2 (Maybe a) Pos
rowcsp animal = \x p -> let y =  do {(po,val) <- animal ; guard (fst p == fst po);return val}  
                        in case elem (Just 1) y of 
                        True -> x == Just 0
                        _    -> case  ((length.filter (== Just 0)) y) == 3 of 
                                True -> x == Just 1
                                _    -> True    

-- |give conditions: e.g. if fst p == 1 , then give the two conditons  
-- |->  Sue's favorite animal is smaller than Ben's.; DiDi's favorite animal is bigger than Sue's.
comparecsp :: (Eq a, Num a) => Animal a -> Constraint2 (Maybe a) Pos
comparecsp animal = \x p -> case fst p of 
                            1 -> helpfun animal p x (<) 3 (4 - snd p) && helpfun animal p x (<) 4 (4 - snd p)
                            2 -> helpfun animal p x (>) 4 (snd p -1)
                            3 -> helpfun animal p x (<) 4 (4 - snd p) && helpfun animal p x (>) 1 (snd p -1) 
                            4 -> helpfun animal p x (<) 2 (4 - snd p) && helpfun animal p x (>) 1 (snd p -1)  && helpfun animal p x (>) 3 (snd p -1)
              where helpfun animal p x f num cond = case (getval animal p f num) of 
                                                      []   -> x == Just 0 
                                                      _    -> case  ((length.filter (== Just 0)) (getval animal p f num) ) == cond of 
                                                                True -> x == Just 0
                                                                _    -> True
                    getval animal p h v = do {(po,val) <- animal ; guard ( fst po == v && h (snd p)  (snd po)) ;return val}                                             
                                          

-- |print one of the Animal Puzzle sulutions
printone :: Animal Int -> IO ()
printone = prettyprint . getpretty . listchar . head . solunew 
                                              where solunew = solu Nothing (map Just [0..1]) [rowcsp, comparecsp]    

-- |print all of the Animal Puzzle solutions
printall :: Animal Int -> IO ()
printall = (mapM_ (prettyprint. getpretty . listchar)) . solunew 
                                              where solunew = solu Nothing (map Just [0..1]) [rowcsp, comparecsp]    

-- |If no solutions print "No Solution"; 
-- |If has solutions, then give the solutions: input Int = 0 -> print one Solution; other wise -> print ALl solutions
-- |In the output, Just 1 means the favorite animal for the corresponding person; 
main :: Int -> Animal Int -> IO()
main i animal = (prettyprint . getpretty . listchar) animal >> case (hasSolution Nothing (map Just [0..1]) [rowcsp, comparecsp] animal ) of    
         True  ->  putStrLn "" >> putStrLn "There is No solution for Animal puzzle"
         _     ->  d >> (if i == 0 then b else c ) >> d >> case i of 
                                     0 -> printone animal
                                     _ -> printall animal
                                  where b = putStrLn "Print one solution "
                                        c = putStrLn "Print all solutions "   
                                        d = putStrLn ""


-- |Helpfunctions in solve the Animal

-- |Change all Aninal from [(b, Maybe Int)] into String
listchar :: Animal Int -> String       
listchar animal = do 
            x <- listsu animal
            case x of 
              Just val -> return $ intToDigit val
              Nothing  -> return '.'

-- |Change String to [String], and add space e.g. "........" -> ["....","....","",""]
getpretty :: String -> [String]
getpretty s
    | s == []   = [] ++ [""] ++ [""] 
    | otherwise = [xs] ++ getpretty ys
    where  xs = take 4 s 
           ys = drop 4 s   

-- |print the results, and add frames (|), (-)in the Output  
prettyprint :: [String] -> IO ()
prettyprint = (mapM_ putStrLn) . addhframe . addvframe               


-- |function addvframe and addhframe :
-- |after using addvframe and addhframe :
--    ....
--    ....
--    ....
--    ....

-- |After using addvframe and addhframe :
--          BCHE
--    Sue  |....
--    Bill |....
--    DiDi |....
--    Ben  |....

--    B: Butterfly; C: Cat; H: Horse ; E: Elephant


addvframe :: [String] -> [String]
addvframe s = addf sa s 
        where sa=["Sue  |","Bill |","DiDi |","Ben  |","","B: Butterfly; C: Cat; H: Horse ; E: Elephant"]
              addf [] [] = []
              addf (x:xs) (y:ys) = (x++y) : addf xs ys 

addhframe :: [String] -> [String]
addhframe s = ["      BCHE"] ++ s
