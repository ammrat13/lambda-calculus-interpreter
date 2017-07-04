{-
  DeBruijnNotation.hs
  By: Ammar Ratnani

  This file provides functions for the parsing of lambda expressions in their 
  original form.
-}

module LambdaCalculus.ParseExpression.StandardNotation (
  parseLamExp
) where

import Text.ParserCombinators.Parsec
import Data.Map (Map)
import qualified Data.Map as Map

import LambdaCalculus.LambdaExpressions


parseLamExp :: Map String (LamExp String) -> String -> Maybe (LamExp String)
parseLamExp binds inp = case parse lamExp inp inp of
                          Left _  -> Nothing  -- Failure
                          Right e -> Just e   -- Success
  where
    lamExp :: Parser (LamExp String)
    lamExp = lam <|> try app <|> parenLamExp <|> var

    parenLamExp :: Parser (LamExp String)
    parenLamExp = do
      spaces >> char '(' >> spaces
      expr <- lamExp
      spaces >> char ')' >> spaces
      return expr
    
    lam :: Parser (LamExp String)
    lam = do
      spaces >> char '\\' >> spaces
      varN <- many1 (noneOf " \\.()")
      spaces >> char '.' >> spaces
      body <- lamExp
      spaces
      return $ Lam varN body
    
    app :: Parser (LamExp String)
    app = let appParam = parenLamExp <|> var in do
      spaces
      e1 <- appParam
      spaces
      e2 <- appParam
      extraExps <- many (spaces >> appParam)  -- Prevent bracket hell
      spaces
      return $ foldl App (App e1 e2) extraExps  -- Recursively apply to all
    
    var :: Parser (LamExp String)
    var = do
      spaces
      varN <- many1 (noneOf " \\.()")
      case Map.lookup varN binds of
        Nothing -> spaces >> return (Var varN)
        Just e  -> spaces >> return e