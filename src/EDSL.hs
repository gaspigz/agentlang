module EDSL where

import AST
import MyMonads

-- ==========================================
-- STATES
-- ==========================================

data StateConfig = StateConfig
    { cfgAction    :: Action      -- La acción a ejecutar
    , cfgOnSuccess :: String      -- Próximo estado si sale bien
    , cfgOnFailure :: String      -- Próximo estado si falla
    } deriving (Show)

defaultConfig :: StateConfig
defaultConfig = StateConfig (Log "No action") "End" "End"

type StateBuilder = State StateConfig

runAction :: Action -> StateBuilder ()
runAction act = modify (\s -> s { cfgAction = act }) -- Crea una copia del record actual pero con datos modificados


onSuccess :: String -> StateBuilder ()
onSuccess next = modify (\s -> s { cfgOnSuccess = next })

onFailure :: String -> StateBuilder ()
onFailure next = modify (\s -> s { cfgOnFailure = next })

-- ==========================================
-- AGENT BUILDER
-- ==========================================

type AgentBuilder = Writer [AgentState]

-- Toma un nombre y un bloque interno (StateBuilder), y genera un AgentState para la lista.
state :: String -> StateBuilder () -> AgentBuilder ()
state name builder = do
    let config = execState builder defaultConfig
    let newState = AgentState {
        stateName = name,
        stateAction = cfgAction config,
        stateTransition = Transition {
            AST.onSuccess = cfgOnSuccess config,
            AST.onFailure = cfgOnFailure config
        }
    }
    -- Agrega el nuevo estado a la lista de estados del agente
    tell [newState]

agent :: AgentBuilder () -> Agent
agent builder = 
    let states = execWriter builder
        -- Asumimos que el primer estado declarado es el inicial (s0)
        initial = case states of
                    (s:_) -> stateName s
                    []    -> "NoStatesDefined"
    in Agent { agentStates = states, agentInitialState = initial }

-- ==========================================
-- HELPERS
-- ==========================================


askUserLine :: String -> StateBuilder ()
askUserLine prompt = runAction (AskUserLine prompt)

-- Escribe el contenido de la memoria en un archivo (Append)
writeBuffer :: FilePath -> StateBuilder ()
writeBuffer path = runAction (WriteBuffer path)

wait :: Int -> StateBuilder ()
wait seconds = runAction (Delay seconds)

-- === MANEJO DE VARIABLES ===

-- Guarda el resultado actual en una variable llamada 'name'
setVar :: String -> StateBuilder ()
setVar name = runAction (SetVar name)

-- Recupera el valor de 'name' y lo deja listo para ser usado
getVar :: String -> StateBuilder ()
getVar name = runAction (GetVar name)