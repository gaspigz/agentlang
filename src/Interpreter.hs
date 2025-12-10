module Interpreter where

import AST
import System.IO
import Control.Exception (try, SomeException, IOException)

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as BS8

import qualified Data.Map as Map

type Success = Bool
type Memory = (String, Map.Map String String)

getCurrentVal :: Memory -> String
getCurrentVal = fst

updateVal :: String -> Memory -> Memory
updateVal newVal (_, vars) = (newVal, vars)



-- Ejecuta una acción individual en el mundo real
executeAction :: Action -> Memory -> IO (Success, Memory)

executeAction (Log msg) mem = do
    putStrLn $ "LOG: " ++ msg
    return (True, mem)

executeAction (ReadFile path) mem = do
    putStrLn $ "Leyendo archivo: " ++ path
    -- Usamos 'try' para capturar errores de IO (ej. archivo no existe)
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left ex -> do
            putStrLn $ "Error leyendo archivo: " ++ show ex
            return (False, mem)
        Right content -> do
            putStrLn $ "Contenido leído (" ++ show (length content) ++ " chars)."
            putStrLn content
            return (True, updateVal content mem) 

executeAction (WriteFile path content) mem = do
    putStrLn $ "Escribiendo en archivo: " ++ path
    result <- try (writeFile path content) :: IO (Either IOException ())
    case result of
        Left ex -> do
            putStrLn $ "Error escribiendo archivo: " ++ show ex
            return (False, mem)
        Right _ -> do
            putStrLn "Escritura exitosa."
            return (True, mem)

executeAction (AskUser prompt) mem = do
    putStrLn $ "Pregunta al usuario: " ++ prompt
    putStr "Respuesta (s/n): "
    hFlush stdout
    resp <- getLine
    return (resp == "s", mem)

executeAction (AskUserLine prompt) mem = do
    putStrLn $ "Pregunta al usuario: " ++ prompt
    putStr "Respuesta: "
    hFlush stdout
    input <- getLine
    return (True, updateVal input mem)

executeAction (WriteBuffer path) mem = do
    putStrLn $ "Escribiendo buffer en archivo: " ++ path
    result <- try (writeFile path (getCurrentVal mem)) :: IO (Either IOException ())
    case result of
        Left ex -> do
            putStrLn $ "Error escribiendo buffer: " ++ show ex
            return (False, mem)
        Right _ -> do
            putStrLn "Buffer escrito exitosamente."
            return (True, mem)

-- VARIABLES:
executeAction (SetVar name) (val, vars) = do
    putStrLn $ "--- Guardando variable '" ++ name ++ "' = " ++ show val
    let newVars = Map.insert name val vars
    return (True, (val, newVars))

executeAction (GetVar name) (val, vars) = do
    putStrLn $ "--- Recuperando variable '" ++ name ++ "'"
    case Map.lookup name vars of
        Nothing -> do
            putStrLn $ "Error: Variable '" ++ name ++ "' no definida."
            return (False, (val, vars))
        Just storedVal -> do
            putStrLn $ "Valor recuperado: " ++ show storedVal
            return (True, (storedVal, vars)) -- El valor recuperado pasa a ser el Actual

-- HTTP:
executeAction (HttpGet url) mem = do
    putStrLn $ "--- [HTTP GET] " ++ url ++ " ---"
    result <- try (do
        request <- parseRequest url
        response <- httpLBS request
        let code = getResponseStatusCode response
        let body = L8.unpack (getResponseBody response)
        return (code, body)
        ) :: IO (Either SomeException (Int, String))
    case result of
        Left ex -> do
            putStrLn $ "Error en HTTP GET: " ++ show ex
            return (False, mem)
        Right (code, body) -> do
            putStrLn $ "Código de respuesta: " ++ show code
            putStrLn $ "Cuerpo de la respuesta: " ++ take 500 body ++ "..."
            return (code >= 200 && code < 300, updateVal body mem)

executeAction (HttpPost url bodyFijo) mem = do
    putStrLn $ "--- [HTTP POST] " ++ url ++ " ---"
    putStrLn $ "Payload: " ++ bodyFijo
    
    result <- try (do
        initReq <- parseRequest url
        let req = setRequestMethod (BS8.pack "POST")
                $ setRequestBodyLBS (L8.pack bodyFijo)
                $ initReq
        response <- httpLBS req
        return (getResponseStatusCode response, L8.unpack (getResponseBody response))
        ) :: IO (Either SomeException (Int, String))

    case result of
        Left ex -> do
            putStrLn $ "Error de Red: " ++ show ex
            return (False, mem)
        Right (code, body) -> do
            putStrLn $ "Status Code: " ++ show code
            putStrLn $ "Response Body: " ++ take 500 body ++ "..."
            return (code >= 200 && code < 300, updateVal body mem)

executeAction (HttpPostMemory url) mem = do
    let bodyMem = getCurrentVal mem
    putStrLn $ "--- [HTTP POST] " ++ url ++ " ---"
    putStrLn $ "Payload from Memory: " ++ take 500 bodyMem ++ "..."

    result <- try (do
        initReq <- parseRequest url
        let req = setRequestMethod (BS8.pack "POST")
                $ setRequestBodyLBS (L8.pack bodyMem)
                $ initReq
        response <- httpLBS req
        return (getResponseStatusCode response, L8.unpack (getResponseBody response))
        ) :: IO (Either SomeException (Int, String))

    case result of
        Left ex -> do
            putStrLn $ "Error de Red: " ++ show ex
            return (False, mem)
        Right (code, body) -> do
            putStrLn $ "Status Code: " ++ show code
            putStrLn $ "Response Body: " ++ take 500 body ++ "..."
            return (code >= 200 && code < 300, updateVal body mem)

-- Busca un estado por nombre en la lista del agente
findState :: String -> [AgentState] -> Maybe AgentState
findState name states = 
    case filter (\s -> stateName s == name) states of
        (s:_) -> Just s
        []    -> Nothing

-- Ejecuta el agente completo
runAgent :: Agent -> IO ()
runAgent agent = loop (agentInitialState agent) ("", Map.empty)
  where
    -- Función recursiva que representa el ciclo de vida del agente
    loop :: String -> Memory -> IO ()
    loop "End" _ = putStrLn "--- Fin de la ejecución ---"
    loop currentStateName currentMem = do
        -- 1. Buscamos la definición del estado actual
        case findState currentStateName (agentStates agent) of
            Nothing -> 
                putStrLn ("Error Fatal: Estado no encontrado " ++ currentStateName)

            Just st -> do
                putStrLn $ "\n--- Estado: " ++ currentStateName ++ " ---"
                
                -- 2. Ejecutamos la acción efectiva
                (success, newMem) <- executeAction (stateAction st) currentMem

                -- 3. Decidimos el próximo estado
                let nextState = if success 
                                then onSuccess (stateTransition st)
                                else onFailure (stateTransition st)
                
                putStrLn $ "Transición -> " ++ nextState
                
                -- 4. Repetimos el ciclo
                loop nextState newMem