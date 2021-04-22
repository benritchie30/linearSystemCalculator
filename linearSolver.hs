import Data.List

data Expr = Exp [Double] Double
  deriving (Eq)

type Matrix = [Expr]

instance Show Expr where
   show (Exp l1 r1) = show (l1 ++ [r1])  

printMatrix :: Matrix -> String
printMatrix m = concatMap (\x -> show x ++ ("\n")) m 

testMatrixFail :: Matrix
testMatrixFail = parser "3 0 0 4 1 0 3 3 4 1 6 7 4"

testMatrixFail2 :: Matrix
testMatrixFail2 = parser "3 0 -1 1 2 0 1 -1 1 0 0 3 8"

preproc :: String -> [[Double]]
preproc inpt = preprocHelper (map read (words inpt))
  where preprocHelper (x:xs) = splitList (round x + 1) xs

splitList :: Int -> [Double] -> [[Double]]
splitList _ [] = []
splitList len xs = a:splitList len b
  where (a,b) = splitAt len xs

preproc2 :: [[Double]] -> Matrix
preproc2 inpt = map (\x -> (Exp (init x) (last x))) inpt

parser :: String -> Matrix
parser inpt = preproc2 (preproc inpt)

scale :: Double -> Expr -> Expr
scale fact (Exp xs x) = Exp (map (\x -> fact * x) xs) (x*fact)

--replaces exp2 + fact * exp1
replace :: Expr -> Expr -> Double -> Expr
replace e1 e2 fact = replace2 e2 (scale fact e1)
  where replace2 :: Expr -> Expr -> Expr
        replace2 (Exp l1 r1) (Exp l2 r2) = Exp (map mySum $ transpose [l1, l2]) (r1 + r2)

-- if I get a really close number to 1 or 0 it will round
mySum :: [Double] -> Double
mySum l1 | s < 1e-5 = 0
         | 1 - s < 1e-5 && 1 - s > 0 = 1 
         | otherwise = sum l1
           where s = abs (sum l1)   


echelon :: Matrix -> Int -> Matrix
echelon m i | i >= length m || not (foldl (\acc x -> checkLine x && acc) True m) = m --not (foldl (\acc x -> checkLine x && acc) True m)
            | otherwise =   echelon (preEchelon (repE) 0) (i + 1)
            where (a,b) = splitAt i m
                  e = head b
                  repE = if (getElem e i == 0) then m else zero e a i ++ [if checkLine r then r else e] ++ zero e (tail b) i
                  r = scale (getFactRed e i) e

getElem :: Expr -> Int -> Double
getElem (Exp l1 r1) i = l1 !! i

checkZerosBelow :: Matrix -> Bool
checkZerosBelow m = foldl (\acc (Exp l1 r1) -> acc && (head l1 == 0)) True (tail m) 

zero :: Expr -> Matrix -> Int -> Matrix
zero e1 m i = (map (\e2-> replace e1 e2 (getFact e1 e2 i)) m)

--gets the factor to zero out rows 
getFact :: Expr -> Expr -> Int -> Double
getFact (Exp l1 r1) (Exp l2 r2) i = negate ((l2 !! i) / (l1 !! i))

--gets the factor for scaling row to one
getFactRed :: Expr -> Int -> Double
getFactRed (Exp l1 r1) i = 1/(l1 !! i)


--if the sum of the absolute value of elements in the list is equal to the last of the list
checkLine :: Expr -> Bool
checkLine (Exp l1 r1) 
       | checkInf (Exp l1 r1) = False
       | checkNoSolution (Exp l1 r1) = False
       | otherwise = True
       -- | otherwise = last l1 == absum
       where  absum = foldl (\acc x -> (abs x) + acc) 0 l1

checkMatrix :: (Expr -> Bool) -> Matrix -> Bool
checkMatrix f m = (foldl (\acc x -> f x || acc) False m)

checkInf :: Expr -> Bool
checkInf (Exp l1 r1) = (absum == 0) && (r1 == 0) 
                     where  absum = foldl (\acc x -> (abs x) + acc) 0 l1

checkNoSolution :: Expr -> Bool
checkNoSolution (Exp l1 r1) = (absum == 0) && (r1 /= 0)
                     where  absum = foldl (\acc x -> (abs x) + acc) 0 l1

getSolutions :: Matrix -> String
getSolutions m = getSolutions' m 0
  where getSolutions' [] i = []
        getSolutions' ((Exp l1 r1):m) i = "x" ++ (show i) ++ " = " ++ (show r1) ++ "\n" ++ getSolutions' m (i+1)

preEchelon :: Matrix -> Int -> Matrix
preEchelon m i | i >= length m = m
               | l2 !! i == 0 = preEchelon (a ++ [cur] ++ tail b) (i + 1)
               | otherwise = m
                     where (a,b) = splitAt i m
                           (Exp l2 r2) = head b
                           nonZ = getNonZeroDiag b i
                           cur = if nonZ == Nothing then (Exp l2 r2) else replace (getJust nonZ) (Exp l2 r2) 1 

getJust :: Maybe a -> a
getJust (Just x) = x

getNonZeroDiag :: Matrix -> Int -> Maybe Expr
getNonZeroDiag m i = find (\(Exp l1 r1) -> l1 !! i /= 0) m


isRowOfZeroes :: Matrix -> Int -> Bool
isRowOfZeroes m i | i >= length m = False
                  | otherwise = foldl (\acc (Exp l1 r1) -> l1 !! i == 0 && acc) True m || isRowOfZeroes m (i+1)

infiniteSolutions :: Matrix -> IO ()
infiniteSolutions m = do 
                      putStrLn "Matrix has infinitely many solutions\n"
                      putStrLn "Gausian Elimination Paused at:"
                      putStrLn (printMatrix m)

noSolutions :: Matrix -> IO ()
noSolutions m = do 
                putStrLn "Matrix has no solutions\n"
                putStrLn "Gausian Elimination Paused at:"
                putStrLn (printMatrix m)

solvable :: Matrix -> IO ()
solvable m = do 
             putStrLn "Recuced Echelon Form:"
             putStrLn (printMatrix m)
             putStrLn "Solutions:"
             putStrLn (getSolutions m)

solve :: Matrix -> IO ()
solve m = do
  let redEchelon = echelon (preEchelon m 0) 0
  if checkMatrix checkInf redEchelon
      then infiniteSolutions redEchelon
  else if checkMatrix checkNoSolution redEchelon
      then noSolutions redEchelon
  else solvable redEchelon

hasError :: IOError -> IO ()
hasError e = putStrLn "Matrix has no solutions\n"

main :: IO ()
main = do
  putStrLn "Enter input file name"
  filename <- getLine
  contents <- readFile filename
  let matrix = parser contents
  putStrLn "\nInput Matrix:" 
  putStrLn (printMatrix matrix)
  solve matrix
  
