module Main where

import AST
import Interpreter
import TypeChecker
import Lexer
import Parser

main :: IO ()
main = do
  text <- getLine
  let tokenList = alexScanTokens text
  let ast = expr tokenList
  let typecorrect = typecheck ast
  case typecorrect of Right r -> case (eval ast) of Right res -> putStrLn $ show res
                                                    Left err -> putStrLn err
                      Left str -> putStrLn $ str
