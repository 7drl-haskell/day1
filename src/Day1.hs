module Day1 where

import GridProto.Classic
import GridProto.Core
import Data.Tuple
import Data.Char (intToDigit)
import Data.Maybe (fromJust)

main :: IO ()
main = runClassic classic

data State = State
  { scene :: Scene
  , playState :: PlayState
  , roomCount :: Int
  } deriving (Show, Eq)

data Scene
  = Scene'Start
  | Scene'Play
  | Scene'GameOver
  deriving (Show, Eq)

data PlayState = PlayState
  { player :: Player
  , rooms :: Map RoomIndex Room
  } deriving (Show, Eq)

data Room = Room
  { doors  :: [Door]
  , walls  :: [(Int,Int)]
  } deriving (Show, Eq)

newtype RoomIndex = RoomIndex Int deriving (Show, Eq, Num, Ord, Enum, Bounded)

data Player = Player
  { playerPos :: (Int, Int)
  , playerRoom :: RoomIndex
  } deriving (Show, Eq)

data Door = Door
  { doorNumber :: Int
  , doorLocation :: (Int, Int)
  } deriving (Show, Eq)

data Dir
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Eq)

classic :: Classic State
classic = Classic
  { title = "Day 1"
  , rows = screenH
  , cols = screenW
  , tilePixelSize = 32
  , backgroundColor = Black2
  , setupFn = return initState
  , updateFn = update
  , cleanupFn = const (return ())
  , tileMapFn = tileMap
  , sfxFn = const []
  , quitFn = const False
  }

screenW, screenH, centerX, centerY :: Int
screenW = 45
screenH = 30
centerX = screenW `div` 2
centerY = screenH `div` 2

initState :: State
initState = State
  { scene = Scene'GameOver
  , playState = PlayState
      { player = Player (0,0) 2
      , rooms = fromList $
          [ ( 0
            , Room
                { doors = [(Door 1 (3,2))]
                , walls = [(1,2), (1,3), (1,4), (1,5), (1,6), (1,7)]
                }
            )
          ] ++ 
          zip [1..9] (repeat $ Room [] [])
      }
  , roomCount = 0
  }

update :: Input -> State -> IO State
update input st
  -- FOR DEBUGGING BELOW
  | pressed 0 = nextRoom 0
  | pressed 1 = nextRoom 1
  | pressed 2 = nextRoom 2
  | pressed 3 = nextRoom 2
  | pressed 4 = nextRoom 5
  | pressed 5 = nextRoom 4
  | pressed 6 = nextRoom 6
  | pressed 7 = nextRoom 7
  | pressed 8 = nextRoom 8
  | pressed 9 = nextRoom 9
  -- FOR DEUGGING ABOVE
  | otherwise = case scene st of
      Scene'Start -> return $ updateStart input st
      Scene'Play -> return $ st
        { playState = (playState st)
          { player = updatePlayer input (player (playState st)) } }
      Scene'GameOver -> return $ updateGameOver input st
  -- FOR DEBUGGING BELOW
  where
    pressed n = lookupKey (keys input) (Char (head $ show n)) == Pressed
    nextRoom n = return $ st { playState = (playState st) { player = (player (playState st)) { playerRoom = n } } }
  -- FOR DEBUGGING ABOVE

updateStart :: Input -> State -> State
updateStart input st = st
  { scene = if isStart then Scene'Play else Scene'Start }
  where
    isStart = lookupKey (keys input) Enter == Pressed || lookupKey (keys input) (Char ' ') == Pressed

updateGameOver :: Input -> State -> State
updateGameOver input st = st
  { scene = if isStart then Scene'Start else Scene'GameOver }
  where
    isStart = lookupKey (keys input) Enter == Pressed || lookupKey (keys input) (Char ' ') == Pressed

updatePlayer :: Input -> Player -> Player
updatePlayer input p = p
  { playerPos = updatePlayerPos input (playerPos p) }

updatePlayerPos :: Input -> (Int, Int) -> (Int, Int)
updatePlayerPos Input{keys=keys} (x,y) = (x',y')
  where
    x'
      | isLeft && isRight = x
      | isLeft = x - 1
      | isRight = x + 1
      | otherwise = x
    y'
      | isUp && isDown = y
      | isUp = y - 1
      | isDown = y + 1
      | otherwise = y
    isLeft = touched left
    isRight = touched right
    isUp = touched up
    isDown = touched down
    touched k = k == Pressed || k == Held
    left = lookupKey keys LeftArrow
    right = lookupKey keys RightArrow
    up = lookupKey keys UpArrow
    down = lookupKey keys DownArrow

tileMap :: State -> Map (Int, Int) Tile
tileMap st = case scene st of
  Scene'Start -> drawStart st
  Scene'Play -> drawPlay st
  Scene'GameOver -> drawGameOver st

drawStart :: State -> Map (Int, Int) Tile
drawStart st = text "DAY 1" White1 (screenW `div` 2 - 2, screenH `div` 2)

drawPlay :: State -> Map (Int, Int) Tile
drawPlay st = mergeTiles clear ( mergeTiles ws ( mergeTiles ds playerTile ) )
  where
    (x,y) = playerPos (player (playState st))
    clear = clearTileMap (colorFromRoomIndex (playerRoom (player (playState st))))
    playerTile = fromList [ ( (x,y), Tile Nothing Nothing (Just wh1) ) ]
    roomIndex = playerRoom (player (playState st))
    room = fromJust $ lookupMap roomIndex (rooms (playState st))
    ds = doorTileMap $ doors room
    ws = wallTileMap $ walls room

drawGameOver :: State -> Map (Int, Int) Tile
drawGameOver st = mergeTiles
  (text roomCountText Red0 (roomCountTextX, centerY))
  (text roomCountValue Red0 (roomCountValueX, centerY + 1))
  where
    roomCountText = "Room Count"
    roomCountTextX = centerX - (length roomCountText) `div` 2
    roomCountValue = show (roomCount st)
    roomCountValueX = centerX - (length roomCountValue) `div` 2

text :: String -> Color -> (Int, Int) -> Map (Int, Int) Tile
text s c (x,y) = fromList (line s)
  where
    line = zipWith tile [0..]
    tile u sym = ((x+u, y), Tile (Just (sym, c)) Nothing Nothing)

clearTileMap :: Color -> Map (Int, Int) Tile
clearTileMap c = fromList
  [ ((x,y), Tile Nothing Nothing (Just c))
  | y <- [0..pred screenH]
  , x <- [0..pred screenW]
  ]

colorFromRoomIndex :: RoomIndex -> Color
colorFromRoomIndex (RoomIndex idx) = colors !! (idx `mod` len)
  where
    colors = colorWheel2
    len = length colors
    
wallTileMap :: [(Int, Int)] -> Map (Int, Int) Tile
wallTileMap locs = fromList $ map (swap . (,) (Tile Nothing Nothing (Just bk2))) locs 

doorTileMap :: [Door] -> Map (Int, Int) Tile
doorTileMap ds = fromList $ map makeDoorTile ds 
  where
    makeDoorTile :: Door -> ((Int, Int), Tile)
    makeDoorTile d = (,) (doorLocation d) $ Tile ( Just (intToDigit $ doorNumber d, bk2) ) ( Just (Square, bk2) ) Nothing

