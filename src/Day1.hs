module Day1 where

import GridProto.Classic
import GridProto.Core

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
  GameMode'Play -> mergeTiles fromList [ ( (x,y), Tile Nothing Nothing (Just wh1) ) ] $ clear

  GameMode'GameOver -> fromList []
  where
    (x,y) = playerPos (player (playState st))
    clear = clearTileMap (colorFromRoomIndex (playerRoom (player (playState st))))

clearTileMap :: Color -> Map (Int, Int) Tile
clearTileMap c = fromList
  [ ((x,y), Tile Nothing Nothing (Just c))
  | y <- [0..pred screenH]
  , x <- [0..pred screenW]
  ]

colorFromRoomIndex :: RoomIndex -> Color
colorFromRoomIndex (RoomIndex idx) = colors !! (idx `mod` len)
  where
    colors = rainbow
    len = length colors

