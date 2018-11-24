module Hooridor.Gui where
import Hooridor.Core
import Hooridor.Ai
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List 
--import Graphics.Gloss.Interface.Pure.Color

type Board = [(Cell,Color)]

data GuiState = GuiState GameState Board

data CellOrWall = Cell' Cell | Wall' Wall deriving (Show)

build :: (Int,Int) -> (Float, Float)
-- TODO: Generalize for size
build (x,y) = ((fromIntegral (x-4)*50),(fromIntegral (y-4)*50))

inverseBuild :: (Float, Float) -> (Int,Int)
--Same as above
inverseBuild (x,y) = (((round (x/50))+4),((round (y/50))+4))

inverseBuild' :: (Float, Float) -> Maybe CellOrWall
-- fix wall placement
inverseBuild' (x, y)
  | row < 0 || row > 8 || col < 0 || row > 8 = Nothing
  | not addRow && not addCol = Just (Cell' (row, col))
  | addRow && not addCol = Just (Wall'
                                  (((row, col), (row + 1, col))
                                  , ((row, col + 1), (row + 1, col + 1))))
  | not addRow && addCol = Just (Wall'
                                  (((row, col), (row, col + 1))
                                  , ((row + 1, col), (row + 1, col + 1))))
  | addRow && addCol = Nothing
  where
    y' = round x + 220 :: Int
    x' = round y + 220 :: Int
    (xDiv, xMod) = divMod x' 50
    (yDiv, yMod) = divMod y' 50
    row = yDiv 
    addRow = yMod > 40
    col = xDiv
    addCol = xMod > 40


colorCell :: (Cell,Color) -> Board
-- Same as above 
colorCell ((x',y'),c) = [((x,y),(chooseColor x y)) 
                            | x<-[0..8], y<-[0..8]]
                                where
                                    chooseColor x y 
                                            | x==x' && y==y' = c
                                            | otherwise = black

-- | Respond to key events.
handleEvents :: Event -> GuiState -> GuiState
handleEvents (EventKey (MouseButton _) Down _ (x', y')) gs =
                    case obj of
                        Just b -> 
                            case b of 
                                Cell' a-> GuiState (takeTurn (MakeMove a) gameState) board
                                Wall' a-> GuiState (takeTurn (PutWall a) gameState) board
                        Nothing -> gs
                    where 
                        (GuiState gameState board) = gs
                        obj = inverseBuild' (x',y')
                        (x,y) = inverseBuild (x',y')

handleEvents (EventMotion (x',y')) gs = 
                    case obj of
                        Just (Cell' a)-> GuiState (gameState) (colorCell (a,dark c))
                        otherwise -> GuiState (gameState) (newBoard)
                    where 
                        (GuiState gameState _) = gs
                        obj = inverseBuild' (x',y')
                        c = colorPlayer (pcolor (currentPlayer gameState))
                        
handleEvents (EventKey (Char 'r') _ _ _) _ = initiateGame 2 8                    
handleEvents _ z = z

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
                    (x,y) = build (xr,yr)

drawCell :: (Cell,Color) -> Picture
drawCell ((x,y),c) = translate x' y' 
                    ((color c (rectangleSolid 40 40)))
                    where 
                        (x',y') = build (x,y)


drawWallSegment :: WallPart -> Color -> Picture
drawWallSegment wp col 
                | abs(x1-x2) == 0 = translate x y (color col (rectangleSolid 40 5)) 
                | otherwise = translate x y (color col (rectangleSolid 5 40))
                        where
                            (c1,c2) = wp
                            (x1,y1) = build c1
                            (x2,y2) = build c2 --TODO Refactor together with next func
                            (x,y) = segmentCoordinates wp

segmentCoordinates :: WallPart -> (Float,Float)
segmentCoordinates (c1,c2) = (x,y)
                        where
                            (x1,y1) = build c1
                            (x2,y2) = build c2
                            x = (x1+x2)/2
                            y = (y1+y2)/2

drawWall :: Color  -> Wall  -> Picture
drawWall c (ws1,ws2) = (drawWallSegment ws1 c) 
                        <> (drawWallSegment ws2 c) 
                        <> translate x y (color black (rectangleSolid 10 10))
                            where 
                                (x1,y1) = segmentCoordinates ws1
                                (x2,y2) = segmentCoordinates ws2
                                x = (x1+x2)/2
                                y = (y1+y2)/2

drawBoard :: Board -> Picture
drawBoard board = (pictures 
            (map drawCell board)) 

drawVictoryScreen :: Color -> Picture
drawVictoryScreen c = translate (-220) 0 (color (dark c) message)
                                where 
                                    message = (scale 0.5 1 (text "You have won!"))

drawScore :: GameState -> Picture
drawScore gs = scale 2 2 (color red (text (show (score gs))))
render :: Int -> GuiState -> Picture
render size (GuiState gameState board) =
                case winner of
                    Nothing ->  translate x y (drawBoard board <>
                                pictures (map drawPlayer ( players)))          
                                <> pictures (map (drawWall red) (walls gameState))  
                                <> drawScore gameState
                    Just p -> drawVictoryScreen (colorPlayer (pcolor p))
                    where 
                        winner = find isWinner players
                        (x,y) = (0,0) --build (-size,-size)
                        players = playerList (gameState)
                        testSegment1 = ((0,1),(1,1))
                        testSegment2 = ((0,0),(1,0))
                        testSegmentHorizontal = ((0,0),(0,1))



-- Create new GuiState with board of given size                        
initiateGame :: Int -> Int -> GuiState
initiateGame pc size = GuiState (initialState pc) newBoard
                            

newBoard :: Board
newBoard = [ ((x,y),black) | x<-[0..8], y<-[0..8]]

update :: Float -> GuiState -> GuiState
update _ state =
        case inteligence player of
            AI -> GuiState (aiPlayer gameState) board
            Human -> state
        where  
            (GuiState gameState board) = state
            player = currentPlayer gameState

playGame :: Int-> Int -> IO ()
playGame size pc = play window background fps (initiateGame pc size) 
            (render size) handleEvents update
