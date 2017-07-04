{-
  ParseFile.hs
  By: Ammar Ratnani

  This file provides a function that parses a file with lambda expressions in 
  either standard notation (.lca) or in DeBruijn Indicies (.ldb).
-}

module LambdaCalculus.ParseFile (
  parseLDBFile,
  parseLCAFile
) where


import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Numeric.Natural
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO

import LambdaCalculus.LambdaExpressions
import LambdaCalculus.ParseExpression.StandardNotation
import LambdaCalculus.ParseExpression.DeBruijnNotation


parseLCAFile :: String -> IO (Maybe (Map String (LamExp String)))
parseLCAFile n = do
  handle <- openFile n ReadMode
  contents <- hGetContents handle
  return $ case parse (lcaParser Map.empty) contents contents of
             Left _  -> Nothing -- Failure
             Right e -> Just e  -- Success
    where
      lcaParser :: Map String (LamExp String) -> Parser (Map String (LamExp String))
      lcaParser binds = do
        many endOfLine  -- Blank lines don't matter
        newExp <- lcaLine binds
        -- Insert the new expression into the binds we recieved
        let newBinds = (Map.insert (fst newExp) (snd newExp) binds)
          -- Keep parsing if there are more expressions
          in lcaParser newBinds <|> return newBinds

      lcaLine :: Map String (LamExp String) -> Parser (String, (LamExp String))
      -- let varN = body
      lcaLine binds = do
        string "let" >> many1 space
        varN <- many (noneOf " =")
        spaces >> char '=' >> spaces
        body <- many (noneOf "\r\n")
        -- Try to parse the body
        case parseLamExp binds body of
          Nothing -> fail ("Failed to parse lambda expression: " ++ body)
          Just e  -> return $ (varN, e)

parseLDBFile :: String -> IO (Maybe (Map String LamExpDB))
parseLDBFile n = do
  handle <- openFile n ReadMode
  contents <- hGetContents handle
  return $ case parse (ldbParser Map.empty) contents contents of
             Left _  -> Nothing -- Failure
             Right e -> Just e  -- Success
    where
      ldbParser :: Map String LamExpDB -> Parser (Map String LamExpDB)
      ldbParser binds = do
        many endOfLine  -- Blank lines don't matter
        newExp <- ldbLine binds
        -- Insert the new expression into the binds we recieved
        let newBinds = (Map.insert (fst newExp) (snd newExp) binds)
          -- Keep parsing if there are more expressions
          in ldbParser newBinds <|> return newBinds

      ldbLine :: Map String LamExpDB -> Parser (String, LamExpDB)
      -- let varN = body
      ldbLine binds = do
        string "let" >> many1 space
        varN <- many (noneOf " =")
        spaces >> char '=' >> spaces
        body <- many (noneOf "\r\n")
        -- Try to parse the body
        case parseLamExpDB binds body of
          Nothing -> fail ("Failed to parse lambda expression: " ++ body)
          Just e  -> return $ (varN, e)