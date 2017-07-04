{-
  DeBruijnNotation.hs
  By: Ammar Ratnani

  This file provides functions for the parsing of lambda expressions in DeBruijn 
  form.
-}

module LambdaCalculus.ParseExpression.DeBruijnNotation (
  parseLamExpDB
) where

import Text.ParserCombinators.Parsec
import Numeric.Natural
import Data.Map (Map)
import qualified Data.Map as Map

import LambdaCalculus.LambdaExpressions


parseLamExpDB :: Map String LamExpDB -> String -> Maybe LamExpDB
parseLamExpDB binds inp = case parse lamExpDB inp inp of
                            Left _  -> Nothing -- Failure
                            Right e -> Just e  -- Success
  where
    lamExpDB :: Parser LamExpDB
    lamExpDB = lam' <|> try app' <|> parenLamExpDB <|> var'

    parenLamExpDB :: Parser LamExpDB
    parenLamExpDB = do
      spaces >> char '(' >> spaces
      expr <- lamExpDB
      spaces >> char ')' >> spaces
      return expr
    
    lam' :: Parser LamExpDB
    lam' = do
      spaces >> char '\\' >> spaces
      body <- lamExpDB
      spaces
      return $ Lam' body
    
    app' :: Parser LamExpDB
    app' = let appParam = parenLamExpDB <|> var' in do
      e1 <- appParam
      spaces
      e2 <- appParam
      extraExps <- many (spaces >> appParam)     -- To prevent bracket hell
      spaces
      return $ foldl App' (App' e1 e2) extraExps -- Recursively apply to all
    
    var' :: Parser LamExpDB
    var' = try bindVar <|> litVar
      where
        bindVar = do
          spaces
          varN <- many1 (noneOf "\\ ()")
          case Map.lookup varN binds of
            Nothing -> fail "Couldn't find bound variable"
            Just e  -> spaces >> return e
        litVar = do
          spaces
          varN <- many1 digit
          spaces
          return $ Var' (read varN :: Natural)