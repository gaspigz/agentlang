module AgentsHaskell.BtcReporterAgent () where

import EDSL
import AST hiding (onSuccess, onFailure)

-- Agente de Reporte Financiero (BTC)
btcReporterAgent :: Agent
btcReporterAgent = agent $ do
    state "Inicio" $ do
        runAction (Log "=== Iniciando Bot de Reporte Financiero (BTC) ===")
        onSuccess "ObtenerPrecio"
        onFailure "End"

    state "ObtenerPrecio" $ do
        runAction (Log "Consultando API pública de CoinDesk (Bitcoin Price Index)...")
        -- Hacemos GET a una API real que devuelve JSON
        runAction (HttpGet "https://min-api.cryptocompare.com/data/generateAvg?fsym=BTC&tsym=USD&e=coinbase")
        onSuccess "GuardarEnVariable"
        onFailure "ErrorRed"

    state "GuardarEnVariable" $ do
        -- Guardamos el JSON obtenido en una variable llamada "raw_btc_data"
        -- Esto es CRUCIAL: protegemos el dato mientras hacemos otras cosas.
        runAction (SetVar "raw_btc_data")
        onSuccess "GuardarLocal"
        onFailure "ErrorIO"

    state "GuardarLocal" $ do
        runAction (Log "Guardando snapshot local en 'agentTxtExample/btc_snapshot.json'...")
        runAction (WriteBuffer "agentTxtExample/btc_snapshot.json")
        onSuccess "PreguntarUsuario"
        onFailure "ErrorIO"

    state "PreguntarUsuario" $ do
        -- Interrumpimos el flujo para decisión humana
        runAction (AskUser "¿Desea subir este reporte al servidor de respaldo remoto?")
        onSuccess "RecuperarDatos" -- Si dice 's', vamos a subirlo
        onFailure "FinLocal"       -- Si dice 'n', terminamos acá

    state "RecuperarDatos" $ do
        runAction (Log "Recuperando datos originales de la variable 'raw_btc_data'...")
        -- AQUÍ ESTÁ LA MAGIA:
        -- Recuperamos el JSON que guardamos hace 3 estados. 
        -- Esto sobreescribe la memoria actual (que tenía la respuesta del usuario 's').
        runAction (GetVar "raw_btc_data")
        onSuccess "SubirNube"
        onFailure "ErrorVar"

    state "SubirNube" $ do
        runAction (Log "Enviando datos a la nube (jsonplaceholder)...")
        -- Enviamos el contenido recuperado (el JSON de BTC) por POST
        runAction (HttpPostMemory "https://jsonplaceholder.typicode.com/posts")
        onSuccess "GuardarRecibo"
        onFailure "ErrorRed"

    state "GuardarRecibo" $ do
        runAction (Log "Respuesta del servidor recibida. Guardando recibo...")
        runAction (WriteBuffer "agentTxtExample/upload_receipt.txt")
        onSuccess "FinRemoto"
        onFailure "ErrorIO"

    state "FinLocal" $ do
        runAction (Log "Proceso finalizado. El reporte solo se guardó localmente.")
        onSuccess "End"
        onFailure "End"

    state "FinRemoto" $ do
        runAction (Log "Proceso finalizado. Reporte subido y respaldado con éxito.")
        onSuccess "End"
        onFailure "End"

    -- Estados de Error
    state "ErrorRed" $ do
        runAction (Log "FALLO CRÍTICO: Error de comunicación HTTP.")
        onSuccess "End"
        onFailure "End"
    
    state "ErrorIO" $ do
        runAction (Log "FALLO CRÍTICO: No se pudo escribir en el disco.")
        onSuccess "End"
        onFailure "End"
        
    state "ErrorVar" $ do
        runAction (Log "FALLO CRÍTICO: Variable no encontrada en memoria.")
        onSuccess "End"
        onFailure "End"