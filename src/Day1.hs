module Day1 where

import GridProto.Classic
import GridProto.Core
import Data.Char
import Data.Tuple

main :: IO ()
main = runClassic classic

data State = State
  { gameMode :: GameMode
  , playState :: PlayState
  } deriving (Show, Eq)

data GameMode
  = GameMode'Start
  | GameMode'Play
  | GameMode'GameOver
  deriving (Show, Eq)

data PlayState = PlayState
  { player :: Player
  } deriving (Show, Eq)

newtype RoomIndex = RoomIndex Int deriving (Show, Eq, Num)

data Player = Player
  { playerPos :: (Int, Int)
  , playerRoom :: RoomIndex
  } deriving (Show, Eq)

classic :: Classic State
classic = Classic
  { title = "Day 1"
  , rows = screenH
  , cols = screenW
  , tilePixelSize = 24
  , backgroundColor = Black2
  , setupFn = return initState
  , updateFn = update
  , cleanupFn = const (return ())
  , tileMapFn = tileMap
  , sfxFn = const []
  , quitFn = const False
  }

screenW, screenH :: Int
screenW = 45
screenH = 30

initState :: State
initState = State
  { gameMode = GameMode'Play
  , playState = PlayState (Player (0,0) 4)
  }

update :: Input -> State -> IO State
update input st = case gameMode st of
  GameMode'Start -> return st
  GameMode'Play -> return $ st
    { playState = (playState st)
      { player = updatePlayer input (player (playState st)) } }
  GameMode'GameOver -> return st

updatePlayer :: Input -> Player -> Player
updatePlayer input p = p
  { playerPos = updatePlayerPos input (playerPos p) }

updatePlayerPos :: Input -> (Int, Int) -> (Int, Int)
updatePlayerPos Input{keys=keys} (x,y) = (x',y')
  where
    x'
      | left == Pressed || left == Held = x - 1
      | right == Pressed || right == Held = x + 1
      | otherwise = x
    y'
      | up == Pressed || up == Held = y - 1
      | down == Pressed || down == Held = y + 1
      | otherwise = y
    left = lookupKey keys LeftArrow
    right = lookupKey keys RightArrow
    up = lookupKey keys UpArrow
    down = lookupKey keys DownArrow

tileMap :: State -> Map (Int, Int) Tile
tileMap st = case gameMode st of
  GameMode'Start -> fromList []
  GameMode'Play -> mergeTiles playerTile ( mergeTiles testDoors ( mergeTiles testWalls clear ) )
  GameMode'GameOver -> fromList []
  where
    (x,y) = playerPos (player (playState st))
    clear = clearTileMap (colorFromRoomIndex (playerRoom (player (playState st))))
    playerTile = fromList [ ( (x,y), Tile Nothing Nothing (Just wh1) ) ]
    testDoors = doorTileMap [1, 2, 3] [(9,8), (10, 13), (3, 7)]
    testWalls = wallTileMap [(0,1), (0,2), (0,3), (0,4), (0,5), (8,1), (8,2), (8,3), (8,4), (8,5), (15, 0), (15, 1), (15, 2), (15, 3), (15, 4)]

clearTileMap :: Color -> Map (Int, Int) Tile
clearTileMap c = fromList
  [ ((x,y), Tile Nothing Nothing (Just c))
  | y <- [0..pred screenH]
  , x <- [0..pred screenW]
  ]

  -- Tried to use TupleSections pragma but it wouldn't work
wallTileMap :: [(Int, Int)] -> Map (Int, Int) Tile
wallTileMap locs = fromList $ map (swap . (,) (Tile Nothing Nothing (Just bk2))) locs 

doorTileMap :: [Int] -> [(Int, Int)] -> Map (Int, Int) Tile
doorTileMap ids locs = fromList 
  [ ((x,y), Tile ( Just (intToDigit i, bk2) ) ( Just (Square, bk2) ) Nothing)
  | i <- ids
  , x <- map fst locs
  , y <- map snd locs
  ]

colorFromRoomIndex :: RoomIndex -> Color
colorFromRoomIndex (RoomIndex idx) = colors !! (idx `mod` len)
  where
    colors = rainbow
    len = length colors

