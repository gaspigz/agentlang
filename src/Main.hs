module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Parser (loadAgentFromFile)
import Interpreter (runAgent)
import AST (Agent) -- Solo necesitamos el tipo

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            putStrLn $ "Cargando archivo AGL: " ++ fileName
            result <- loadAgentFromFile fileName
            
            case result of
                Left err -> do
                    putStrLn "Error de sintaxis (Parsing):"
                    print err
                    exitFailure
                    
                Right agentAST -> do
                    putStrLn "Parseo exitoso. Ejecutando agente..."
                    runAgent agentAST

        _ -> do
            putStrLn "Uso correcto: stack run -- <archivo.agl>"
            putStrLn "Ejemplo: stack run -- script.agl"