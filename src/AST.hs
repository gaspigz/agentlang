module AST where

data Action =
    ReadFile FilePath
    | WriteFile FilePath String
    | WriteBuffer FilePath
    | DeleteFile FilePath
    | Log String
    | AskUser String
    | AskUserLine String
    | HttpGet String
    | HttpPost String String
    | HttpPostMemory String
    | HttpGetToken String String         -- url, token
    | HttpPostToken String String String -- url, token, bodyFijo
    | HttpPostBufferToken String String  -- url, token (usa memoria como body)
    -- Variables (usando libreria containers Map)
    | SetVar String   
    | GetVar String
    -- Tiempos
    | Delay Int
    --Ejecutables (.sh)
    deriving (Show, Eq)

data Transition = 
    Transition {
        onSuccess :: String,
        onFailure :: String
    } deriving (Show, Eq)

data AgentState = 
    AgentState {
        stateName :: String,
        stateAction :: Action,
        stateTransition :: Transition
    } deriving (Show, Eq)

data Agent =
    Agent {
        agentInitialState :: String,
        agentStates :: [AgentState]
    } deriving (Show, Eq)