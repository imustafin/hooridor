module Hooridor.Ai.Helper where
import Hooridor.Core

-- | Generate all possible turns for current player
allTurns :: GameState -> [Turn]
allTurns state = (generateWalls state) ++ (validMoves state)

-- | Generate all possible wall placement turns
generateWalls :: GameState -> [Turn]
generateWalls state = possibleTurns
            where
                possibleTurns = filter (\a -> validTurnBot a state) (map PutWall possibleWalls)
                possibleWalls = map generateWall (concatMap generatePart board)

-- | Check if turn is valid, but don't use
validTurnBot :: Turn -> GameState -> Bool
validTurnBot (PutWall wall) state = 
    hasWalls &&
    isInBounds &&
    not intersect &&
    isValidWall wall 
    where
        current = currentPlayer state
        ((cell1,cell2), (cell3, cell4)) = wall
        isInBounds = all cellInBound [cell1, cell2, cell3, cell4]
        wall' = transposeWall wall
        coincide (part1, part2)
            = hasWallPart part1 wall || hasWallPart part2 wall
        hasWalls = wallsLeft current > 0
        intersect = any (\w -> coincide w || wallEq w wall') (walls state)
        

board :: [Cell]
board = [ (x,y) | x<-[0..8], y<-[0..8]]

-- | Generate all possible wall parts given cell
generatePart :: Cell -> [WallPart]
generatePart (x,y) = horizontal ++ vertical
              where 
                horizontal = [((x,y),(x,y + 1))]
                vertical = [((x,y),(x + 1,y))]

-- | Generate all possible wall given wall part
generateWall :: WallPart -> Wall
generateWall this 
                | x2 /= x1 = (this,vertical)
                | otherwise = (this,horizontal)
              where
                ((x1,y1),(x2,y2)) = this
                horizontal = ((x1 + 1,y1),(x2 + 1,y2))
                vertical = ((x1,y1 + 1),(x2,y2 + 1))
