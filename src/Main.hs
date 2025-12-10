module Main where

import AST hiding (onSuccess, onFailure)
import EDSL
import Interpreter (runAgent)
import Agents

main :: IO ()
main = do
    putStrLn "Iniciando Agente: "
    runAgent postExampleAgent