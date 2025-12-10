module Parser (loadAgentFromFile) where

import Text.Parsec
import Text.Parsec.String (Parser)
import AST

-- 1. LEXER BÁSICO

whitespace :: Parser ()
whitespace = skipMany (space <|> tab <|> newline)

quotedString :: Parser String
quotedString = do
    _ <- char '"'
    content <- many (noneOf "\"")
    _ <- char '"'
    whitespace
    return content

integer :: Parser Int
integer = do
    digits <- many1 digit
    whitespace
    return (read digits)

symbol :: String -> Parser String
symbol s = do
    str <- string s
    whitespace
    return str

identifier :: Parser String
identifier = do
    first <- letter
    rest <- many (alphaNum <|> char '_')
    whitespace
    return (first:rest)

-- 2. PARSEO DE ACCIONES (Estilo explícito con 'do')

parseAction :: Parser Action
parseAction = choice 
    [ parseLog
    , parseReadFile
    , parseWriteFile
    , parseWriteBuffer
    , parseAskUserLine
    , parseAskUser
    , parseSetVar
    , parseGetVar
    , parseHttpGet
    , parseHttpPostMemory
    , parseHttpPost
    , parseDelay
    ]

-- Definimos cada parser por separado para máxima claridad

parseLog :: Parser Action
parseLog = do
    _ <- try (symbol "Log")
    msg <- quotedString
    return (Log msg)

parseReadFile :: Parser Action
parseReadFile = do
    _ <- try (symbol "ReadFile")
    path <- quotedString
    return (ReadFile path)

parseWriteFile :: Parser Action
parseWriteFile = do
    _ <- try (symbol "WriteFile")
    path <- quotedString
    content <- quotedString
    return (WriteFile path content)

parseWriteBuffer :: Parser Action
parseWriteBuffer = do
    _ <- try (symbol "WriteBuffer")
    path <- quotedString
    return (WriteBuffer path)

parseAskUserLine :: Parser Action
parseAskUserLine = do
    _ <- try (symbol "AskUserLine")
    prompt <- quotedString
    return (AskUserLine prompt)

parseAskUser :: Parser Action
parseAskUser = do
    _ <- try (symbol "AskUser")
    prompt <- quotedString
    return (AskUser prompt)

parseSetVar :: Parser Action
parseSetVar = do
    _ <- try (symbol "SetVar")
    name <- quotedString
    return (SetVar name)

parseGetVar :: Parser Action
parseGetVar = do
    _ <- try (symbol "GetVar")
    name <- quotedString
    return (GetVar name)

parseHttpGet :: Parser Action
parseHttpGet = do
    _ <- try (symbol "HttpGet")
    url <- quotedString
    return (HttpGet url)

parseHttpPostMemory :: Parser Action
parseHttpPostMemory = do
    _ <- try (symbol "HttpPostMemory")
    url <- quotedString
    return (HttpPostMemory url)

parseHttpPost :: Parser Action
parseHttpPost = do
    _ <- try (symbol "HttpPost")
    url <- quotedString
    body <- quotedString
    return (HttpPost url body)

parseDelay :: Parser Action
parseDelay = do
    _ <- try (symbol "Delay")
    seconds <- integer
    return (Delay seconds)

-- 3. PARSEO DE ESTADOS
-- Estructura: STATE Nombre { ACTION ... SUCCESS ... FAILURE ... }

parseState :: Parser AgentState
parseState = do
    _ <- symbol "STATE"
    name <- identifier
    _ <- symbol "{"

    _ <- symbol "ACTION"
    act <- parseAction

    _ <- symbol "SUCCESS"
    sTarget <- identifier

    _ <- symbol "FAILURE"
    fTarget <- identifier

    _ <- symbol "}"

    -- Construcción explícita sin usar $
    let trans = Transition { onSuccess = sTarget, onFailure = fTarget }
    return (AgentState 
        { stateName = name
        , stateAction = act
        , stateTransition = trans
        })

-- 4. PARSEO DEL AGENTE COMPLETO
-- Estructura: AGENT Nombre { STATE ... STATE ... }

parseAgent :: Parser Agent
parseAgent = do
    whitespace 
    _ <- symbol "AGENT"
    _ <- identifier
    _ <- symbol "{"
    states <- many1 parseState
    _ <- symbol "}"
    eof
    
    let initialState = case states of
                         (s:_) -> stateName s
                         []    -> "End"
                         
    return (Agent { agentStates = states, agentInitialState = initialState })

-- 5. FUNCIÓN EXPORTADA

loadAgentFromFile :: FilePath -> IO (Either ParseError Agent)
loadAgentFromFile path = do
    content <- readFile path
    return (runParser parseAgent () path content)