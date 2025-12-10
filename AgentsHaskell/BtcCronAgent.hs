module AgentsHaskell.BtcCronAgent () where

import EDSL
import AST hiding (onSuccess, onFailure)

btcCronAgent :: Agent
btcCronAgent = agent $ do
    state "Inicio" $ do
        runAction (Log "=== [CRON] Iniciando ciclo de reporte BTC ===")
        onSuccess "ObtenerPrecio"
        onFailure "ErrorGrave"

    state "ObtenerPrecio" $ do
        runAction (Log "1. Obteniendo precio actualizado...")
        -- Usamos la API que pasaste
        runAction (HttpGet "https://min-api.cryptocompare.com/data/generateAvg?fsym=BTC&tsym=USD&e=coinbase")
        onSuccess "GuardarVar"
        onFailure "ErrorRed"

    state "GuardarVar" $ do
        -- Guardamos el JSON crudo en variable para no perderlo
        runAction (SetVar "btc_current")
        onSuccess "GuardarLocal"
        onFailure "ErrorGrave"

    state "GuardarLocal" $ do
        runAction (Log "2. Sobreescribiendo archivo local...")
        -- Esto actualizará el archivo con el último precio
        runAction (WriteBuffer "agentTxtExample/btc_live_monitor.json")
        onSuccess "RecuperarParaEnvio"
        onFailure "ErrorIO"

    state "RecuperarParaEnvio" $ do
        -- Recuperamos la variable para asegurarnos de enviar el JSON limpio
        runAction (GetVar "btc_current")
        onSuccess "SubirNube"
        onFailure "ErrorGrave"

    state "SubirNube" $ do
        runAction (Log "3. Enviando datos a jsonplaceholder...")
        -- Enviamos sin pedir confirmación
        runAction (HttpPostMemory "https://jsonplaceholder.typicode.com/posts")
        onSuccess "EsperarCiclo"
        onFailure "ErrorRed"

    state "EsperarCiclo" $ do
        runAction (Log "Ciclo terminado con éxito.")
        -- Esperamos 30 segundos
        wait 30 
        -- === EL LOOP: Volvemos al inicio en lugar de ir a End ===
        onSuccess "Inicio" 
        onFailure "ErrorGrave"

    -- Estados de Error (Estos sí terminan el programa para no loopear errores infinitamente)
    state "ErrorRed" $ do
        runAction (Log "Error de conexión. Reintentando en 10 segundos...")
        wait 10
        onSuccess "Inicio" -- Reintento automático
        onFailure "ErrorGrave"

    state "ErrorIO" $ do
        runAction (Log "Error de escritura en disco.")
        onSuccess "ErrorGrave"
        onFailure "ErrorGrave"

    state "ErrorGrave" $ do
        runAction (Log "Error irrecuperable. El Cron se detiene.")
        onSuccess "End"
        onFailure "End"