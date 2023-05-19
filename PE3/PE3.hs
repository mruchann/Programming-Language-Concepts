{-# LANGUAGE FlexibleInstances #-}

module PE3 where

import Data.List (sort, sortBy)
import Text.Printf (printf)

data Term = Const Integer | Pw Integer Power | Trig Integer Power Trigonometric | Exp Integer Power Exponential

data Power = Power Integer
data Polynomial = Polynomial [(Integer, Power)]
data Exponential = Exponential Polynomial
data Trigonometric = Sin Polynomial | Cos Polynomial

class Evaluable a where
    function :: a -> (Integer -> Double)

class Differentiable a where
    derivative :: a -> [Term]

getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- INSTANCES FOR POWER

-- data Power = Power Integer

instance Show Power where
    show (Power 0) = "1"
    show (Power 1) = "x"
    show (Power n) = "x^" ++ show n

instance Eq Power where
    (Power l) == (Power r) = l == r

instance Ord Power where
    (Power l) <= (Power r) = l <= r
    (Power l) >= (Power r) = l >= r
    (Power l) < (Power r) = l < r
    (Power l) > (Power r) = l > r

instance Evaluable Power where
    function x = case x of
        Power n -> \x -> getRounded (fromInteger (x ^ n))

instance Differentiable Power where
    derivative x = case x of
        Power 0 -> [Const 0]
        Power 1 -> [Const 1]
        Power n -> [Pw n (Power (n - 1))]

-- INSTANCES FOR POLYNOMIAL

-- data Polynomial = Polynomial [(Integer, Power)]

instance Show Polynomial where
    show (Polynomial []) = ""

    show (Polynomial [(0, _)]) = ""

    show (Polynomial [(coefficient, Power 0)]) = show coefficient

    show (Polynomial [(-1, power)]) = "-" ++ show power
    show (Polynomial [(1, power)]) = show power

    show (Polynomial [(coefficient, power)]) = show coefficient ++ show power
    show (Polynomial ((coefficient, power):xs)) =  
        if not (null xs)
        then show (Polynomial [(coefficient, power)]) ++ " + " ++ show (Polynomial xs)
        else show (Polynomial [(coefficient, power)])


instance Evaluable Polynomial where
    function x = case x of
        Polynomial [] -> \x -> 0.0
        Polynomial [(coefficient, power)] -> \x -> getRounded (fromInteger coefficient * function power x)
        Polynomial ((coefficient, power):xs) -> \x -> getRounded (fromInteger coefficient * function power x + function (Polynomial xs) x)

instance Differentiable Polynomial where
    derivative x = case x of
        Polynomial [] -> []
        Polynomial [(coefficient, Power n)] -> if n == 0 
            then [] -- head problem
            else [Pw (coefficient * n) (Power (n - 1))]
        Polynomial ((coefficient, Power n):xs) -> if n == 0 
            then derivative (Polynomial xs)
            else Pw (coefficient * n) (Power (n - 1)) : derivative (Polynomial xs)

-- INSTANCES FOR TRIGONOMETRIC

--data Trigonometric = Sin Polynomial | Cos Polynomial

instance Show Trigonometric where
    show trigonometric = case trigonometric of
        Sin polynomial -> case polynomial of 
            Polynomial [] -> ""
            Polynomial [(0, Power 0)] -> "0"
            Polynomial [(coefficient, Power 0)] -> "sin" ++ show (Polynomial [(coefficient, Power 0)])
            Polynomial [(coefficient, Power 1)] -> "sin" ++ show (Polynomial [(coefficient, Power 1)])
            Polynomial ((coefficient, power):xs) -> if not (null xs) 
                then "sin(" ++ show (Polynomial [(coefficient, power)]) ++ " + " ++ show (Polynomial xs) ++ ")"
                else "sin(" ++ show (Polynomial [(coefficient, power)]) ++ ")"
        Cos polynomial -> case polynomial of 
            Polynomial [] -> ""
            Polynomial [(0, Power 0)] -> "0"
            Polynomial [(coefficient, Power 0)] -> "cos" ++ show (Polynomial [(coefficient, Power 0)])
            Polynomial [(coefficient, Power 1)] -> "cos" ++ show (Polynomial [(coefficient, Power 1)])
            Polynomial ((coefficient, power):xs) -> if not (null xs) 
                then "cos(" ++ show (Polynomial [(coefficient, power)]) ++ " + " ++ show (Polynomial xs) ++ ")"
                else "cos(" ++ show (Polynomial [(coefficient, power)]) ++ ")"

instance Evaluable Trigonometric where
    function x = case x of 
        Sin polynomial -> \x -> getRounded (sin(function polynomial x))
        Cos polynomial -> \x -> getRounded (cos(function polynomial x))

instance Differentiable Trigonometric where
    derivative x = case x of 
        Sin polynomial -> [combine (Trig 1 (Power 0) (Cos polynomial)) term | term <- derivativeList]

            where
                derivativeList = derivative polynomial
                combine :: Term -> Term -> Term
                combine (Trig coefficient power trigonometric) term = case term of
                    Pw n power -> Trig (coefficient*n) power trigonometric
                    Const n -> Trig (coefficient*n) (Power 0) trigonometric

        Cos polynomial -> [combine (Trig (-1) (Power 0) (Sin polynomial)) term | term <- derivativeList] 
            where 
                derivativeList = derivative polynomial
                combine :: Term -> Term -> Term
                combine (Trig coefficient power trigonometric) term = case term of
                    Pw n power -> Trig (coefficient*n) power trigonometric
        
-- INSTANCES FOR EXPONENTIAL

--data Exponential = Exponential Polynomial

instance Show Exponential where
    show (Exponential (Polynomial [(0, _)])) = "1"
    show (Exponential (Polynomial [(1, Power 0)])) = "e"
    show (Exponential (Polynomial [(coefficient, Power 0)])) = "e^" ++ show (Polynomial [(coefficient, Power 0)])
    show (Exponential (Polynomial [(coefficient, Power 1)])) = "e^" ++ show (Polynomial [(coefficient, Power 1)])
    show (Exponential (Polynomial ((coefficient, power):xs))) = if not (null xs) 
        then "e^(" ++ show (Polynomial [(coefficient, power)]) ++ " + " ++ show (Polynomial xs) ++ ")"
        else "e^(" ++ show (Polynomial [(coefficient, power)]) ++ ")"

instance Evaluable Exponential where
    function x = case x of 
        Exponential polynomial -> \x -> getRounded (exp(function polynomial x))

instance Differentiable Exponential where
    derivative x = case x of 
        Exponential polynomial -> [combine (Exp 1 (Power 0) (Exponential polynomial)) term | term <- derivativeList] 

            where 
                derivativeList = derivative polynomial
                combine :: Term -> Term -> Term
                combine (Exp coefficient power exponential) term = case term of
                    Pw n power -> Exp (coefficient*n) power exponential
        
-- INSTANCES FOR TERM

-- data Term = Const Integer | Pw Integer Power | Trig Integer Power Trigonometric | Exp Integer Power Exponential

instance Show Term where
    show term = case term of
        Const coefficient -> show coefficient
        Pw coefficient power -> show (Polynomial [(coefficient, power)])
        Trig coefficient power trigonometric ->
            if power == Power 0 && coefficient == 1 
                then show trigonometric
            else if power == Power 0 && coefficient == -1 
                then "-" ++ show trigonometric
            else show (Polynomial [(coefficient, power)]) ++ show trigonometric
        Exp coefficient power exponential -> 
            if power == Power 0 && coefficient == 1 
                then show exponential
            else if power == Power 0 && coefficient == -1 
                then show "-" ++ show exponential
            else show (Polynomial [(coefficient, power)]) ++ show exponential

instance Evaluable Term where
    function x = case x of 
        Const coefficient -> \x -> getRounded (fromInteger coefficient)
        Pw coefficient power -> \x -> getRounded (fromInteger coefficient * function power x)
        Trig coefficient power trigonometric -> \x -> getRounded (fromInteger coefficient * function power x * function trigonometric x)
        Exp coefficient power exponential -> \x -> getRounded (fromInteger coefficient * function power x * function exponential x)

skipZeros :: [Term] -> [Term]
skipZeros [] = []
skipZeros (x:xs) = case x of 
    Const n -> if n == 0 
        then skipZeros xs
        else x : skipZeros xs
    Pw coefficient  (Power n) -> case coefficient of 
        0 -> skipZeros xs
        otherwise -> x : skipZeros xs
    Trig coefficient (Power n) trigonometric -> case coefficient of 
        0 -> skipZeros xs
        otherwise -> x : skipZeros xs
    Exp coefficient (Power n) exponential -> case coefficient of 
        0 -> skipZeros xs
        otherwise -> x : skipZeros xs

instance Differentiable Term where
    derivative x = case x of 
        Const coefficient -> [Const 0]
        Pw coefficient (Power n) -> case n of
            0 -> [Const 0]
            1 -> [Const coefficient]
            otherwise -> [Pw (coefficient*n) (Power(n - 1))]

        Trig coefficient (Power n) trigonometric -> case trigonometric of 
            
            Sin polynomial -> skipZeros (combine (head (derivative (Pw coefficient (Power n)))) (Trig 1 (Power 0) (Sin polynomial))
                                :
                            [combine (Pw coefficient (Power n)) trig | trig <- derivative (Sin polynomial)])
                where 
                    combine :: Term -> Term -> Term
                    combine (Const coefficient) (Trig 1 (Power 0) (Sin polynomial)) = Trig coefficient (Power 0) (Sin polynomial)
                    combine (Const coefficient) (Trig 1 (Power 0) (Cos polynomial)) = Trig coefficient (Power 0) (Cos polynomial)

                    combine (Const coefficient1) (Trig coefficient2 (Power n) (Sin polynomial)) = Trig (coefficient1*coefficient2) (Power n) (Sin polynomial)
                    combine (Const coefficient1) (Trig coefficient2 (Power n) (Cos polynomial)) = Trig (coefficient1*coefficient2) (Power n) (Cos polynomial)

                    combine (Pw coefficient1 (Power n1)) (Trig coefficient2 (Power n2) (Sin polynomial)) = Trig (coefficient1*coefficient2) (Power (n1+n2)) (Sin polynomial)
                    combine (Pw coefficient1 (Power n1)) (Trig coefficient2 (Power n2) (Cos polynomial)) = Trig (coefficient1*coefficient2) (Power (n1+n2)) (Cos polynomial)

            Cos polynomial -> skipZeros (combine (head (derivative (Pw coefficient (Power n)))) (Trig 1 (Power 0) (Cos polynomial))
                                :
                            [combine (Pw coefficient (Power n)) trig | trig <- derivative (Cos polynomial)])
                where 
                    combine :: Term -> Term -> Term
                    combine (Const coefficient) (Trig 1 (Power 0) (Sin polynomial)) = Trig coefficient (Power 0) (Sin polynomial)
                    combine (Const coefficient) (Trig 1 (Power 0) (Cos polynomial)) = Trig coefficient (Power 0) (Cos polynomial)

                    combine (Const coefficient1) (Trig coefficient2 (Power n) (Sin polynomial)) = Trig (coefficient1*coefficient2) (Power n) (Sin polynomial)
                    combine (Const coefficient1) (Trig coefficient2 (Power n) (Cos polynomial)) = Trig (coefficient1*coefficient2) (Power n) (Cos polynomial)

                    combine (Pw coefficient1 (Power n1)) (Trig coefficient2 (Power n2) (Sin polynomial)) = Trig (coefficient1*coefficient2) (Power (n1+n2)) (Sin polynomial)
                    combine (Pw coefficient1 (Power n1)) (Trig coefficient2 (Power n2) (Cos polynomial)) = Trig (coefficient1*coefficient2) (Power (n1+n2)) (Cos polynomial)

        Exp coefficient (Power n) exponential -> case exponential of
            Exponential polynomial -> skipZeros (combine (head (derivative (Pw coefficient (Power n)))) (Exp 1 (Power 0) (Exponential polynomial))
                                :
                            [combine (Pw coefficient (Power n)) trig | trig <- derivative (Exponential polynomial)])
                where 
                    combine :: Term -> Term -> Term
                    combine (Const coefficient) (Exp 1 (Power 0) (Exponential polynomial)) = Exp coefficient (Power 0) (Exponential polynomial)

                    combine (Const coefficient1) (Exp coefficient2 (Power n) (Exponential polynomial)) = Exp (coefficient1*coefficient2) (Power n) (Exponential polynomial)

                    combine (Pw coefficient1 (Power n1)) (Exp coefficient2 (Power n2) (Exponential polynomial)) = Exp (coefficient1*coefficient2) (Power (n1+n2)) (Exponential polynomial)

-- INSTANCES FOR [TERM]

-- data Term = Const Integer | Pw Integer Power | Trig Integer Power Trigonometric | Exp Integer Power Exponential

instance Evaluable [Term] where
    function terms = \x -> getRounded (sum [function term x | term <- terms])

instance Differentiable [Term] where
    derivative x = case x of 
        [] -> []
        (term:xs) -> derivative term ++ derivative xs