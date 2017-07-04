{-
  Main.hs
  By: Ammar Ratnani

  This file will take a lambda calculus file in standard notation (.lca) or in 
  DeBruijn Index notation (.ldb), parse it, then output the fully reduced "main" 
  expression the the screen.

  Each file has many lines in the form:
    `let varName = body`
  . Blank lines don't do anything. Once the file is read, the expression 
  corresponding to "main" in place of `varName` is reduced and outputted to the 
  screen.
-}

import System.Environment
import Data.Map (Map)
import qualified Data.Map as Map

import LambdaCalculus.ParseFile
import LambdaCalculus.LambdaExpressions

main :: IO ()
main = do
  fn <- getArgs >>= (\as -> return $ as !! 0) -- File name is first argument
  case reverse fn of
    ('a':'c':'l':'.':_) -> printLCAMain fn  -- Ends in ".lca"
    ('b':'d':'l':'.':_) -> printLDBMain fn  -- Ends in ".ldb"
  where
    printLDBMain :: String -> IO ()
    printLDBMain fn = do
      res <- parseLDBFile fn  -- Maybe (Map String LamExpDB)
      case res of
        Nothing -> putStrLn $ "Failed to parse file: " ++ fn
        Just es -> if Map.notMember "main" es
                     then putStrLn $ "No main expression in: " ++ fn
                     else print $ fullyReduce (es Map.! "main") -- Evaluate "main"
    
    printLCAMain :: String -> IO ()
    printLCAMain fn = do
      res <- parseLCAFile fn  -- Maybe (Map String (LamExp String))
      case res of
        Nothing -> putStrLn ("Failed to parse file: " ++ fn)
        Just es -> if Map.notMember "main" es
                     then putStrLn $ "No main expression in: " ++ fn
                     else case toDB (es Map.! "main") of
                            Nothing -> putStrLn $ "Main expression has unbound variables: " ++ show (es Map.! "main")
                            Just de -> print de -- Try to convert, then evaluate