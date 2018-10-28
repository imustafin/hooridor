module Hooridor.Gui where
import Hooridor.Core
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


-- | Respond to key events.
handleEvents :: Event -> GameState -> GameState
handleEvents _ = id

window :: Display
window = InWindow "Hooridor? A?" (600, 600) (0, 0)

background :: Color
background = white

fps :: Int
fps = 60

colorPlayer :: PlayerColor -> Color
colorPlayer Red = red
colorPlayer Green = green
colorPlayer Yellow = yellow
colorPlayer Orange = orange

drawPlayer :: Player -> Picture
drawPlayer p = translate x y (color (colorPlayer (pcolor p)) (circleSolid 15))
                where 
                    (xr,yr) = pos p
                    x = fromIntegral xr*50
                    y = fromIntegral yr*50

renderCell :: Cell -> Picture
renderCell (x,y) = translate (fromIntegral x*50) (fromIntegral y*50) 
                    ((rectangleSolid 40 40))

drawBoard :: Int -> Picture
drawBoard m = (pictures 
            (map renderCell [ (x,y) | x<-[0..m], y<-[0..m]]) ) 
             
                
                

drawWall :: Wall -> Picture
drawWall ((c1,c2),(c3,c4)) = translate nx1  ny1
                                    (rectangleSolid 10 10)
                    where
                        (x1,y1) = c1 
                        nx1 = (fromIntegral x1*50-25) 
                        ny1 = (fromIntegral y1*50-25) 

render :: Int -> GameState -> Picture
render size state = translate x x (drawBoard size <>
                pictures (map drawPlayer (playerList state)))
                    where 
                        x = fromIntegral (-(size*25))
                        wall = (((0,1),(1,1)),((1,1),(2,2)))

testCells :: Int -> Int -> [Cell]
testCells n m = [ ((x,x)) | x <- [0..n] ] 
update :: Float -> GameState -> GameState
update _ = id

testState :: GameState 
testState = initialState 4

testPlayer :: Player
testPlayer =  Player { pcolor = Red, pos = (0,4), wallsLeft = 5}

playGame :: Int-> Int -> IO ()
playGame size pc = play window background fps (initialState pc) 
            (render size) handleEvents update
