{- TicTacToe ---------------------------------------------------------------------------------------

Plantilla de código para el trabajo de aplicación de la UT2 del curso de 2020 de
_Programación Funcional_ para las carreras de Ingeniería y Licenciatura en Informática de la FIT
(UCU).
Los docentes no garantizan que este código esté libre de errores. De encontrar problemas, por favor
reportarlos a la cátedra.

Leonardo Val, Ignacio Pacheco.
-}
module TicTacToe where

import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex)
import System.Random (randomRIO)

{- Es posible que el paquete `System.Random` no esté disponible si se instaló el core de la Haskell
Platform en el sistema. Para instalarlo, ejecutar los siguientes comandos:

> cabal update
> cabal install random

La herramienta `cabal` es un manejador de paquetes usado por la plataforma Haskell. Debería estar
disponible junto con el `ghci`.

-}

{-- Lógica de juego --------------------------------------------------------------------------------

Funciones de marca sin ninguna implementación útil. Reemplazar por el código apropiado o por imports
a los módulos necesarios.
-}

type TicTacToePlayer = String
playerX = "X"
playerO = "O"
noPlayer = ""

type TicTacToeGame = [TicTacToePlayer]
type TicTacToeAction = Int --que sea entre 1-9

beginning :: TicTacToeGame
beginning = take 9 (repeat noPlayer)

activePlayer :: TicTacToeGame -> TicTacToePlayer
activePlayer g
   | winner g playerX || winner g playerO = noPlayer
   | length (filter (==noPlayer) g) > 0 && mod (length (filter (==noPlayer) g)) 2 == 1 =  playerX
   | length (filter (==noPlayer) g) > 0 && mod (length (filter (==noPlayer) g)) 2 == 0 =  playerO
   | otherwise = noPlayer

actions :: TicTacToeGame -> [(TicTacToePlayer, [TicTacToeAction])]
actions g
   | activePlayer g == playerX = [(playerX, (movimientos g)), (playerO, [])]
   | otherwise = [(playerX, []), (playerO, (movimientos g))]
      where
         movimientos g
            | g == [] = []
            | noPlayer == head g = 9 - (length g) : movimientos (tail g)
            | otherwise = movimientos (tail g)

setActionAt :: TicTacToeGame -> TicTacToePlayer -> TicTacToeAction -> TicTacToeGame
setActionAt g player n
      | n == 0 = player : tail g
      | n < (length g) = head g : setActionAt (tail g) player (n-1)
      | otherwise = error "Ingrese una posicion existente "

next :: TicTacToeGame -> (TicTacToePlayer, TicTacToeAction) -> TicTacToeGame
next g (player, action)
   | activePlayer g == player && g!!action == noPlayer = setActionAt g player action
   | otherwise = error "!"

winner :: TicTacToeGame -> TicTacToePlayer -> Bool
winner g player
   | g!!0 == player && g!!1 == player && g!!2 == player = True
   | g!!3 == player && g!!4 == player && g!!5 == player = True
   | g!!6 == player && g!!7 == player && g!!8 == player = True
   | g!!0 == player && g!!3 == player && g!!6 == player = True
   | g!!1 == player && g!!4 == player && g!!7 == player = True
   | g!!2 == player && g!!5 == player && g!!8 == player = True
   | g!!0 == player && g!!4 == player && g!!8 == player = True
   | g!!6 == player && g!!4 == player && g!!2 == player = True
   | otherwise = False

result :: TicTacToeGame -> [(TicTacToePlayer, Int)]
result g
   | winner g playerX = [(playerX, 1), (playerO, (-1))]
   | winner g playerO = [(playerX, (-1)), (playerO, 1)]
   | snd( last (actions g) ) == snd( head (actions g) ) = [(playerX, 0), (playerO, 0)]
   | otherwise = []
      where


showBoard :: TicTacToeGame -> String
showBoard g
   | g == [] = []
   | mod (length g) 3 == 1 = ( cambiar (head g) ++ "\n")++ showBoard (tail g)
   | otherwise = (cambiar (head g)) ++ showBoard (tail g)
      where
         cambiar x
            | x == noPlayer = "."
            | otherwise = x


showAction :: TicTacToeAction -> String
showAction a = show a

readAction :: String -> TicTacToeAction
readAction s = read s

test :: [TicTacToeGame]
test = [["X","O","O","X","","","X","",""], beginning, ["X","X","X","O","","","O","",""], ["X","O","O","","X","","","","X"], ["X","O","X","O","X","O","X","X","O"], ["X","O","O","X","","","X","",""], ["X","","","","","","","",""] , ["X","O","","","","","","",""] , ["X","O","X","O","X","O","X","O","X"] , ["X","O","X","X","O","O","X","O","X"], ["X","X","O","","","","","",""]]

players :: [TicTacToePlayer]
players = [playerX, playerO]

{-- Match controller -------------------------------------------------------------------------------

Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.

-}
type TicTacToeAgent = TicTacToeGame -> IO (Maybe TicTacToeAction)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (TicTacToeAgent, TicTacToeAgent) -> TicTacToeGame -> IO [(TicTacToePlayer, Int)]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showBoard g)
   let p = activePlayer g
   if p == noPlayer then return (result g)
   else do
     let ag = [ag1, ag2] !! (fromJust (elemIndex p players))
     move <- ag g
     runMatch ags (next g (p, fromJust move))

{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.
-}
runOnConsole :: IO [(TicTacToePlayer, Int)]
runOnConsole = do
   runMatch (consoleAgent playerX, consoleAgent playerO) beginning

{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.
-}
consoleAgent :: TicTacToePlayer -> TicTacToeAgent
consoleAgent player state = do
   let moves = fromJust $ lookup player (actions state)
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move:" ++ concat [" "++ showAction m | m <- moves])
      line <- getLine
      let input = readAction line
      if elem input moves then return (Just input) else do
         putStrLn "Invalid move!"
         consoleAgent player state

randomAgent :: TicTacToePlayer -> TicTacToeAgent
randomAgent player state = do
    let moves = fromJust $ lookup player (actions state)
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))

-- Fin
