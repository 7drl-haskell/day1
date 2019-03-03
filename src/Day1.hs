module Day1 where

import GridProto.Classic
import GridProto.Core

main :: IO ()
main = runClassic classic

data State = State
  { scene :: Scene
  , playState :: PlayState
  } deriving (Show, Eq)

data Scene
  = Scene'Start
  | Scene'Play
  | Scene'GameOver
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
  { scene = Scene'Start
  , playState = PlayState (Player (0,0) 4)
  }

update :: Input -> State -> IO State
update input st = case scene st of
  Scene'Start -> return $ updateStartScene input st
  Scene'Play -> return $ st
    { playState = (playState st)
      { player = updatePlayer input (player (playState st)) } }
  Scene'GameOver -> return st

updateStartScene :: Input -> State -> State
updateStartScene input st = st
  { scene = if isStart then Scene'Play else Scene'Start
  }
  where
    isStart = lookupKey (keys input) Enter == Pressed || lookupKey (keys input) (Char ' ') == Pressed

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
tileMap st = case scene st of
  Scene'Start -> text "DAY 1" White1 (screenW `div` 2 - 2, screenH `div` 2)
  Scene'Play -> mergeTiles clear $ fromList [ ( (x,y), Tile Nothing Nothing (Just wh1) ) ]
  Scene'GameOver -> fromList []
  where
    (x,y) = playerPos (player (playState st))
    clear = clearTileMap (colorFromRoomIndex (playerRoom (player (playState st))))

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

