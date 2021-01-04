
module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

window :: Display
window = InWindow "Snake Game" (1280, 720) (100, 100)

background :: Color
background = black

render :: GameState -> Picture
render gameState = pictures $   [ fillRectangle red (32, 0) (1280, 20)
                                , fillRectangle red (32, 36) (1280, 20)
                                , fillRectangle red (0, 18) (20, 720)
                                , fillRectangle red (64, 18) (20, 720) ] ++
                                  fmap (convertToPicture white) snake ++ 
                                  fmap (convertToPicture green) [food] ++
                                  gameOverPicture
    where   snake = getSnake gameState 
            food = getFood gameState
            convertToPicture :: Color -> (Int, Int) -> Picture
            convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) (20, 20)
            fillRectangle color' (tx, ty) (w, h) =  color color' $ 
                                                    scale 1 (-1) $ 
                                                    translate (tx * 20 - 640) (ty * 20 - 360) $ 
                                                    rectangleSolid w h
            toFloat (x, y) = (fromIntegral x, fromIntegral y)
            gameOverPicture =   if (isGameOver gameState) 
                                then [  color red  $ 
                                        translate (-200) (0) $ 
                                        scale 0.5 0.5 $ 
                                        text "GAME OVER"
                                     ,  color red $ 
                                        translate (-175) (-50) $ 
                                        scale 0.2 0.2 $ 
                                        text "Press SPACE to try again." ] 
                                else []
                                                        
update :: Float -> GameState -> GameState
update seconds gameState =  if (gameOver) 
                            then gameState
                            else GameState newSnake newFood' direction newGameOver newStdGen
    where   snake = getSnake gameState 
            food = getFood gameState
            direction = getDirection gameState
            gameOver = isGameOver gameState
            stdGen = getRandomStdGen gameState
            (wasFoodEaten, newSnake) = move food direction snake
            (newFood, newStdGen) = generateNewFood newSnake stdGen
            newFood' =  if wasFoodEaten 
                        then newFood
                        else food
            newGameOver = checkGameOver newSnake

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) gameState = if (getDirection gameState == RIGHT)
                                                                 then gameState
                                                                 else changeDirection gameState LEFT



handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = if (getDirection gameState == LEFT)
                                                                 then gameState
                                                                 else changeDirection gameState RIGHT 


handleKeys (EventKey (SpecialKey KeyUp   ) Down _ _) gameState = if (getDirection gameState == DOWN)
                                                                 then gameState
                                                                 else changeDirection gameState UP 


handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) gameState = if (getDirection gameState == UP)
                                                                 then gameState
                                                                 else changeDirection gameState DOWN 


handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState =    if (isGameOver gameState)
                                                                    then initialGameState False
                                                                    else gameState
handleKeys _ gameState = gameState

main :: IO ()
main = play window background 10 (initialGameState True) render handleKeys update
