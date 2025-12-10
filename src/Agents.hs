module Agents where

import EDSL
import AST hiding (onSuccess, onFailure)

loggerAgent :: Agent
loggerAgent = agent $ do
    state "Start" $ do
        runAction (Log "Agente iniciado.")
        onSuccess "AskUser"
        onFailure "End"
    state "AskUser" $ do
        runAction (AskUser "¿Desea continuar?")
        onSuccess "LogContinue"
        onFailure "LogStop"
    state "LogContinue" $ do
        runAction (Log "El usuario decidió continuar.")
        onSuccess "End"
        onFailure "End"
    state "LogStop" $ do
        runAction (Log "El usuario decidió detenerse.")
        onSuccess "End"
        onFailure "End"

agenteEscritorDinamico :: Agent
agenteEscritorDinamico = agent $ do
    state "Inicio" $ do
        runAction (Log "Iniciando sistema de notas...")
        onSuccess "Menu"
        onFailure "End"

    state "Menu" $ do
        runAction (AskUser "¿Desea agregar una nueva linea al archivo?")
        onSuccess "PedirDato"   -- Si dice 's', vamos a pedir el dato
        onFailure "Despedida"   -- Si dice 'n', terminamos

    state "PedirDato" $ do
        askUserLine "Escriba su nota: "
        onSuccess "GuardarDato"
        onFailure "Error"

    state "GuardarDato" $ do
        writeBuffer "notas.txt"
        onSuccess "Confirmacion"
        onFailure "Error"

    state "Confirmacion" $ do
        runAction (Log "Nota guardada correctamente.")
        onSuccess "Menu" -- Volvemos al loop
        onFailure "End"

    state "Error" $ do
        runAction (Log "Ocurrió un error escribiendo el archivo.")
        onSuccess "End"
        onFailure "End"
        
    state "Despedida" $ do
        runAction (Log "Gracias por usar el sistema. Adios.")
        onSuccess "End"
        onFailure "End"

-- Agente de Prueba HTTP Real
httpTestAgent :: Agent
httpTestAgent = agent $ do
    state "Inicio" $ do
        runAction (Log "=== Iniciando Test de Conectividad HTTP ===")
        onSuccess "HacerGet"
        onFailure "End"

    state "HacerGet" $ do
        runAction (Log "1. Ejecutando GET a jsonplaceholder...")
        runAction (HttpGet "https://jsonplaceholder.typicode.com/posts/1") 
        onSuccess "EscribirArchivo"
        onFailure "Error"
    
    state "EscribirArchivo" $ do
        runAction (Log "2. Escribiendo respuesta en 'respuesta_get.txt'...")
        runAction (WriteBuffer "agentTxtExample/respuesta_get.txt")
        onSuccess "FinExito"
        onFailure "Error"

    state "FinExito" $ do
        runAction (Log "=== Test finalizado correctamente ===")
        onSuccess "End"
        onFailure "End"

    state "Error" $ do
        runAction (Log "FALLO: Error de comunicación con la API.")
        onSuccess "End"
        onFailure "End"

postExampleAgent :: Agent
postExampleAgent = agent $ do
    state "Inicio" $ do
        runAction (Log "=== Iniciando Test de POST HTTP ===")
        onSuccess "HacerPost"
        onFailure "End"

    state "HacerPost" $ do
        runAction (Log "1. Ejecutando POST a jsonplaceholder...")
        runAction (HttpPost 
            "https://jsonplaceholder.typicode.com/posts"
            "{ \"title\": \"Agente\", \"body\": \"Probando POST\", \"userId\": 99 }"
            )
        onSuccess "GuardarRespuesta"
        onFailure "Error"

    state "GuardarRespuesta" $ do
        runAction (Log "2. Guardando la respuesta en 'respuesta_post.txt'...")
        runAction (WriteBuffer "agentTxtExample/respuesta_post.txt")
        onSuccess "FinExito"
        onFailure "Error"

    state "FinExito" $ do
        runAction (Log "=== POST finalizado correctamente ===")
        onSuccess "End"
        onFailure "End"

    state "Error" $ do
        runAction (Log "FALLO: Error de comunicación en POST.")
        onSuccess "End"
        onFailure "End"

