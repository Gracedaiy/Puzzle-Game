module Sudoku where
import Csp  
import Data.Char
import Control.Monad
import Control.Applicative 


-- |------------------------------------------------------------------------------------------------------
-- |Following is one example for  sudoku problem 
type Nat      = Int 
type Pos      = (Nat,Nat)
type Sudoku a = Puzzle (Maybe a) Pos

-- |load from an input file as type of String and Change it to a Sudoku problem
load :: [Int] -> String -> Sudoku Int
load xs s = do
             (line,s1)   <- zip xs $ lines s
             (column,c1) <- zip xs s1 
             case c1 of 
                '.' -> return ((line,column), Nothing)
                c   -> let i = digitToInt c in return ((line,column),Just i)

--- |row constraint function : in each row, the valued filled should be different with one values existed 
onerow :: (Eq a) => Sudoku a -> Constraint2 (Maybe a) Pos
onerow sudoku = comfun sudoku (notElem) g
                where g p1 p2 = fst p1 == fst p2 

-- |column constraint function: in each column, the valued filled should be different with one values existed 
onecolumn :: (Eq a) => Sudoku a -> Constraint2 (Maybe a) Pos
onecolumn sudoku = comfun sudoku (notElem) g
                where g p1 p2 = snd p1 == snd p2  


-- |oneblock constraint function: in each 3x3 block, the valued filled should be different with one values existed 
oneblock :: Eq a => Sudoku a -> Constraint2 (Maybe a) Pos
oneblock sudoku = comfun sudoku (notElem) g
                where g p1 p2 = let (i1,j1) = rangefun p1 fst
                                    (i2,j2) = rangefun p1 snd in 
                                    fst p2 >=i1 && fst p2 <=j1 && snd p2 >=i2 && snd p2 <=j2

-- |print one of the Sudoku sulutions
printone :: String -> IO ()
printone = prettyprint . getpretty . listchar . head . solunew. load [1..9]   
                                              where solunew = solu Nothing (map Just [1..9]) [onerow, onecolumn, oneblock]                   
-- |print all of the Sudoku solutions
printall :: String -> IO ()
printall = (mapM_ (prettyprint. getpretty . listchar)) . solunew . load [1..9] 
                                              where solunew = solu Nothing (map Just [1..9]) [onerow, onecolumn, oneblock]  

-- |Read the problem from an .txt file and if no solutions print "No Solution"; 
-- If has solutions, then give the solutions: input Int = 0 -> print one Solution; other wise -> print ALl solutions
main :: Int -> IO()
main a = do
    i <- readFile "sudoku2.txt" 
    prettyprint . lines $ i
    case ((hasSolution Nothing (map Just [1..9]) [onerow, onecolumn, oneblock] ) . load [1..9] $ i) of    
         True  ->  putStrLn "" >> putStrLn "There is No solution for sudoku puzzle"
         _     ->  d >> (if a == 0 then b else c ) >> d >>  readFile "sudoku2.txt" >>= \j-> case a of 
                                     0 -> printone j
                                     _ -> printall j
                                  where b = putStrLn "Print one solution "
                                        c = putStrLn "Print all solutions "   
                                        d = putStrLn ""
-- |Helpfunctions in solve the Sudoku

-- |determine the postion of 3x3 block: e.g, b=(2,5), the block in line 1~3, column 4~6
rangefun :: b -> (b -> Nat) -> (Nat,Nat)
rangefun p f  
    | j == 0     = (3*i-2,3*i)
    | otherwise  = (3*(i+1)-2,3*(i+1))
    where  i = div (f p) 3
           j = mod (f p) 3

-- |Change all Sudoku from [(b, Maybe Int)] into String
listchar :: Sudoku Int -> String       
listchar su = do 
            Just val <- listsu su
            return $ intToDigit val

-- |Change String to [String], and add space 
getpretty :: String -> [String]
getpretty s
    | s == []   = [] ++ [""] ++ [""] 
    | otherwise = [xs] ++ getpretty ys
    where  xs = take 9 s 
           ys = drop 9 s   

-- |print the results, and add frames (|), (-)in the Output  
prettyprint :: [String] -> IO ()
prettyprint = (mapM_ putStrLn) . addhframe . addvframe                 


-- |function addvframe and addhframe : add frames (|), (-)in the Output 
-- |after using addvframe and addhframe :
--    .519.3...
--    32......5
--    .69....3.
--    ..2.9.143
--    945...762
--    173.6.8..
--    .3....58.
--    8.......6
--    ...7..3..

-- |After using addvframe and addhframe :
--    .51|9.3|...
--    32.|...|..5
--    .69|...|.3.
--    --- --- ---
--    ..2|.9.|143
--    945|...|762
--    173|.6.|8..
--    --- --- ---
--    .3.|...|58.
--    8..|...|..6
--    ...|7..|3..

addvframe :: [String] -> [String]
addvframe s = s >>= \xs -> 
    case xs of 
         "" ->  return xs 
         _  ->  let 
                    (ys1,zs1) = splitAt 3 xs  
                    (ys2,zs2) = splitAt 3 zs1
                 in  
                    return (ys1 ++ "|" ++ ys2 ++"|" ++ zs2)

addhframe :: [String] -> [String]
addhframe s = 
    let 
        (ys1,zs1) = splitAt 3 s  
        (ys2,zs2) = splitAt 3 zs1
    in  
        ys1 ++ s2 ++ ys2 ++ s2 ++ zs2
    where s2 = ["--- --- ---"]   



