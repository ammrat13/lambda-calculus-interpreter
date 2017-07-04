{-
  LambdaExpressions.hs
  By: Ammar Ratnani

  This file provides functions and types for dealing with Lambda Expressions in 
  both their original formulation and in DeBruijn form. It implements alpha 
  equivalence for both forms, and provides beta and eta reductions for DeBruijn 
  form.
-}

module LambdaCalculus.LambdaExpressions(
  LamExp(..),
  LamExpDB(..),
  toDB,
  etaReduce,
  betaReduce,
  fullyReduce
) where

import Numeric.Natural
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad


-- Definition of a Lambda Expression
data LamExp a = Var a
              | Lam a (LamExp a)
              | App (LamExp a) (LamExp a)

instance (Ord a) => Eq (LamExp a) where
  -- For DeBruijn Indicies, alpha-equivalence is synctactic equivalence
  (==) e1 e2 = toDB e1 == toDB e2

instance (Show a) => Show (LamExp a) where
  show (Var x) = show x
  show (Lam x e) = "\\ " ++ show x ++ " -> " ++ show e
  -- Only show parenthesis if it is not a variable
  show (App (Var x) (Var y)) = show x ++ " " ++ show y
  show (App (Var x) e) = show x ++ " (" ++ show e ++ ")"
  show (App e (Var x)) = "(" ++ show e ++ ") " ++ show x
  show (App e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"


-- Definition of DeBuijn Indicies
data LamExpDB = Var' Natural
              | Lam' LamExpDB
              | App' LamExpDB LamExpDB
  -- For DeBruijn Indicies, alpha-equivalence is synctactic equivalence
  deriving Eq

instance Show LamExpDB where
  show (Var' n) = show n
  show (Lam' e) = "\\ " ++ show e
  -- Only show parenthesis if it is not a variable
  show (App' e1 e2) = (if isAllVars e1 then show e1 else "(" ++ show e1 ++ ")")
      ++ " " ++ (if isAllVars e2 then show e2 else "(" ++ show e2 ++ ")")
    where
      isAllVars (Var' _) = True
      isAllVars (Lam' _) = False
      isAllVars (App' e1 e2) = isAllVars e1 && isAllVars e2

toDB :: (Ord a) => LamExp a -> Maybe LamExpDB
toDB e = go 0 Map.empty e
  where
    go :: (Ord a) => Natural -> Map a Natural -> LamExp a -> Maybe LamExpDB
    go d ds (Var a) = case Map.lookup a ds of
                        Just d' -> Just $ Var' (d-1-d') -- Prevent off by 1
                        Nothing -> Nothing -- Can't have unbounded variables
    -- Lambdas increment the depth
    go d ds (Lam a e) = (liftM Lam') (go (d+1) (Map.insert a d ds) e)
    -- Applications distributed over
    go d ds (App e1 e2) = (liftM2 App') (go d ds e1) (go d ds e2)


-- Reduction Procedures

etaReducePass :: LamExpDB -> Maybe LamExpDB
etaReducePass (Lam' (App' e (Var' 0))) = Just e
etaReducePass _ = Nothing -- Fail if can't be reduced

etaReduce :: LamExpDB -> Maybe LamExpDB
etaReduce e = go True e
  where go first e = case etaReducePass e of
                       Nothing  -> if first then Nothing else Just e
                       Just ere -> go False ere

betaReducePass :: LamExpDB -> Maybe LamExpDB
betaReducePass (App' (Lam' b) s) = Just $ go 0 b
  where
    go d o@(Var' n) = case compare n d of
                          GT -> Var' (n-1) -- Decrement frees
                          EQ -> s
                          LT -> o
    -- Lambdas increment the depth
    go d (Lam' b) = Lam' $ go (d+1) b
    -- Distribute over applications
    go d (App' e1 e2) = App' (go d e1) (go d e2)
betaReducePass (App' e s) = betaReducePass e >>= (\e' -> betaReducePass (App' e' s))
betaReducePass _ = Nothing  -- Fail if it can't be reduced

betaReduce :: LamExpDB -> Maybe LamExpDB
betaReduce e = go True e
  where go first e = case betaReducePass e of
                       Nothing  -> if first then Nothing else Just e
                       Just bre -> go False bre

-- This one won't fail
fullyReduce :: LamExpDB -> LamExpDB
fullyReduce e = (tryMay etaReduce) $ (tryMay betaReduce) e
  where
    -- Returns the parameter if the result of the function is `Nothing`
    tryMay :: (a -> Maybe a) -> a -> a
    tryMay f x = case f x of
                   Nothing -> x
                   Just y  -> y