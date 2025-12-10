module AST where

data Action =
    ReadFile FilePath
    | WriteFile FilePath String
    | WriteBuffer FilePath
    | DeleteFile FilePath
    | Log String
    | AskUser String
    | AskUserLine String
    | RunDynamicAgent
    | HttpGet String
    | HttpPost String String
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