module Day1 where

import qualified Data.Set as S
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Data.Tuple
import Data.Char (intToDigit)
import Data.Maybe (fromJust, isJust, fromMaybe)
import GridProto.Classic
import GridProto.Core
import System.Random (randomIO, randomRIO, Random)

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
  , nextDir :: Maybe Dir
  , tick :: Int
  , rooms :: Map RoomIndex Room
  , roomCount :: Int
  , changedRoom :: Bool
  } deriving (Show, Eq)

data Room = Room
  { doors   :: Map (Int, Int) Door
  , walls   :: S.Set (Int, Int)
  , enemies :: Map (Int, Int) Enemy
  } deriving (Show, Eq)

newtype RoomIndex = RoomIndex { unRoomIndex :: Int }
  deriving (Show, Eq, Num, Ord, Enum, Bounded, Random)

data Player = Player
  { playerPos :: (Int, Int)
  , playerRoom :: RoomIndex
  , playerHp :: Int
  } deriving (Show, Eq)

data Door = Door
  { doorToRoom :: RoomIndex
  } deriving (Show, Eq)

data Enemy = Enemy 
  { enemyAtk :: Int 
  } deriving (Show, Eq)

data Dir
  = U
  | D
  | L
  | R
  deriving (Show, Eq)

classic :: Classic State
classic = Classic
  { title = "Day 1"
  , rows = screenH
  , cols = screenW
  , tilePixelSize = 24
  , backgroundColor = Black2
  , setupFn = generateInitState
  , updateFn = update
  , cleanupFn = const (return ())
  , tileMapFn = tileMap
  , sfxFn = sfx
  , quitFn = const False
  }

sfx :: State -> [Sfx]
sfx st = (addDamageSfx st . addDoorSfx st) []

addDoorSfx :: State -> [Sfx] -> [Sfx]
addDoorSfx State{playState=PlayState{changedRoom}} sfxs = if changedRoom then SfxDoor : sfxs else sfxs

addDamageSfx :: State -> [Sfx] -> [Sfx]
addDamageSfx State{playState=PlayState{player, rooms}} sfxs = if Map.member playerLoc es && hp > 0 then SfxDamage : sfxs else sfxs
  where
    playerLoc = playerPos $ player 
    roomIndex = playerRoom $ player 
    room = fromJust $ lookupMap roomIndex rooms
    es = enemies room
    hp = playerHp player

addSfx :: Sfx -> [Sfx] -> [Sfx]
addSfx sfx sfxs = sfx : sfxs

screenW, screenH, centerX, centerY, hudW, maxHp :: Int
gameW   = 44
gameH   = 30
hudW    = 1
screenW = gameW + hudW
screenH = gameH
centerX = screenW `div` 2
centerY = screenH `div` 2
maxHp   = 100

initState :: State
initState = State
  { scene = Scene'Start
  , playState = PlayState
      { player = Player (0,0) 0 50
      , nextDir = Nothing
      , tick = 0
      , rooms = fromList $ []
      , roomCount = 0
      , changedRoom = False
      }
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
      Scene'Play -> return $ if hp <= 0 then st { scene = Scene'GameOver } else st { playState = stepTick $ updatePlayerHp $ updatePlayState input (sameRoom $ playState st) }
      Scene'GameOver -> updateGameOver input st
  -- FOR DEBUGGING BELOW
  where
    pressed n = lookupKey (keys input) (Char (head $ show n)) == Pressed
    nextRoom n = return $ st { playState = (playState st) { player = (player (playState st)) { playerRoom = n } } }
    hp = playerHp $ player $ playState st 
  -- FOR DEBUGGING ABOVE

updateStart :: Input -> State -> State
updateStart input st = st
  { scene = if isStart then Scene'Play else Scene'Start }
  where
    isStart = lookupKey (keys input) Enter == Pressed || lookupKey (keys input) (Char ' ') == Pressed

updateGameOver :: Input -> State -> IO State
updateGameOver input st = if isStart then generateInitState else return $ st { scene = Scene'GameOver }
  where
    isStart = lookupKey (keys input) Enter == Pressed || lookupKey (keys input) (Char ' ') == Pressed

updatePlayerHp :: PlayState -> PlayState
updatePlayerHp pState = case lookupMap currPos es of
  Just Enemy{enemyAtk} -> pState { player = p { playerHp = ( playerHp p ) - enemyAtk } }
  Nothing -> pState
  where
    p = player pState
    currPos = playerPos p
    roomIndex = playerRoom p
    room = fromJust $ lookupMap roomIndex (rooms pState)
    es = enemies room

stepTick :: PlayState -> PlayState
stepTick pState = pState { tick = (tick pState + 1) `mod` 6 }

sameRoom :: PlayState -> PlayState
sameRoom pState = pState { changedRoom = False }

updatePlayState :: Input -> PlayState -> PlayState
updatePlayState input pState
  | tick pState /= 0 = pState { nextDir = dirFromInput input <|> nextDir pState }
  | S.member nextPos ws = pState { nextDir = Nothing }
  | otherwise = case lookupMap currPos ds of
      Just Door{doorToRoom} -> if roomIndex == doorToRoom
        then movedPlayer
        else nextRoom . toEnum $ unRoomIndex doorToRoom
      Nothing -> movedPlayer
  where
    movedPlayer = pState 
      { player = p { playerPos = nextPos }
      , nextDir = Nothing
      }
    p = player pState
    currPos = playerPos p
    nextPos = fromMaybe currPos (applyDir currPos <$> nextDir pState)
    roomIndex = playerRoom p
    room = fromJust $ lookupMap roomIndex (rooms pState)
    ws = walls room
    ds = doors room
    nextRoom n = pState { player = ( player pState ) { playerRoom = n }, roomCount = ( roomCount pState + 1 ), changedRoom = True  }

applyDir :: (Int, Int) -> Dir -> (Int, Int)
applyDir (x,y) d = (inX $ x + x', inY $ y + y')
  where
    (x', y') = fromDir d
    inX = min (pred gameW) . max 0
    inY = min (pred gameH) . max 0

fromDir :: Dir -> (Int, Int)
fromDir d = case d of
  U -> (0,-1)
  D -> (0,1)
  L -> (-1,0)
  R -> (1,0)

updatePlayerPos :: Input -> (Int, Int) -> (Int, Int)
updatePlayerPos input (x,y) = (inX $ x' + x, inY $ y' + y)
  where
    (x',y') = case dirFromInput input of
      Nothing -> (0,0)
      Just U -> (0,-1)
      Just D -> (0,1)
      Just L -> (-1,0)
      Just R -> (1,0)
    inX = min (pred gameW) . max 0
    inY = min (pred gameH) . max 0

tileMap :: State -> Map (Int, Int) Tile
tileMap st = case scene st of
  Scene'Start -> drawStart st
  Scene'Play -> drawPlay st
  Scene'GameOver -> drawGameOver st

drawStart :: State -> Map (Int, Int) Tile
drawStart st = text "DAY 1" White1 (gameW `div` 2 - 2, gameH `div` 2)

drawPlay :: State -> Map (Int, Int) Tile
drawPlay st = placeTilesAt ( mergeTiles clear ( mergeTiles ws ( mergeTiles ds ( mergeTiles es playerTile ) ) ) ) (gameW, 0) (drawHUD hp)
  where
    (x,y) = playerPos (player (playState st))
    clear = clearTileMap (colorFromRoomIndex (playerRoom (player (playState st))))
    playerTile = fromList [ ( (x,y), Tile Nothing Nothing (Just wh1) ) ]
    roomIndex = playerRoom (player (playState st))
    room = fromJust $ lookupMap roomIndex (rooms (playState st))
    ds = doorTileMap $ doors room
    ws = wallTileMap $ walls room
    es = enemyTileMap $ enemies room
    hp = playerHp $ player $ playState st

drawHUD :: Int -> Map (Int, Int) Tile
drawHUD hp = placeTilesAt hpTileMap (0, maxHpTiles + 1) hpLabelMap 
  where   
    maxHpTiles = (maxHp `div` 10) - 1 
    hpTiles = hp `div` 10   
    cutoff = maxHpTiles - hpTiles
    locList = (0,) <$> [0..(maxHpTiles)]
    emptyTile = Tile Nothing (Just (Square, rs0)) Nothing
    fullTile = Tile Nothing (Just (FillSquare, rs0)) Nothing
    hpTileMap = fromList $ map (\(x,y) -> if y <= cutoff then ((x, y), emptyTile) else ((x, y), fullTile)) locList
    hpLabelMap = fromList $ [((0, 0), Tile ( Just ('H', rs0) ) Nothing Nothing), ((0, 1), Tile ( Just ('P', rs0) ) Nothing Nothing)]

drawGameOver :: State -> Map (Int, Int) Tile
drawGameOver st = mergeTiles
  (text roomCountText Red0 (roomCountTextX, centerY))
  (text roomCountValue Red0 (roomCountValueX, centerY + 1))
  where
    roomCountText = "Room Count"
    roomCountTextX = centerX - (length roomCountText) `div` 2
    roomCountValue = show (roomCount $ playState st)
    roomCountValueX = centerX - (length roomCountValue) `div` 2

text :: String -> Color -> (Int, Int) -> Map (Int, Int) Tile
text s c (x,y) = fromList (line s)
  where
    line = zipWith tile [0..]
    tile u sym = ((x+u, y), Tile (Just (sym, c)) Nothing Nothing)

clearTileMap :: Color -> Map (Int, Int) Tile
clearTileMap c = fromList
  [ ((x,y), Tile Nothing Nothing (Just c))
  | y <- [0..pred gameH]
  , x <- [0..pred gameW]
  ]

colorFromRoomIndex :: RoomIndex -> Color
colorFromRoomIndex (RoomIndex idx) = colors !! (idx `mod` len)
  where
    colors = colorWheel2
    len = length colors

wallTileMap :: S.Set (Int,Int) -> Map (Int, Int) Tile
wallTileMap = fromList . (map ((,Tile Nothing Nothing (Just bk2)))) . S.toList 

doorTileMap :: Map (Int, Int) Door -> Map (Int, Int) Tile
doorTileMap = fmap intToDoorTile 
  where
    intToDoorTile :: Door -> Tile
    intToDoorTile Door{doorToRoom} = Tile
      (Just (intToDigit $ unRoomIndex doorToRoom, bk2))
      (Just (Square, bk2))
      (Just $ colorFromRoomIndex doorToRoom)

enemyTileMap :: Map (Int, Int) Enemy -> Map (Int, Int) Tile
enemyTileMap = fmap enemyToTile
  where 
    enemyToTile :: Enemy -> Tile
    enemyToTile e = Tile Nothing Nothing (Just rd1) 

dirFromInput :: Input -> Maybe Dir
dirFromInput Input{keys} 
    | (isLeft || isRight) && (isUp || isDown) = Nothing
    | isLeft && isRight = Nothing
    | isUp && isDown = Nothing
    | isLeft = Just L
    | isRight = Just R
    | isUp = Just U
    | isDown = Just D
    | otherwise = Nothing
  where
    isLeft = touched left
    isRight = touched right
    isUp = touched up
    isDown = touched down
    touched k = k == Pressed || k == Held
    left = lookupKey keys LeftArrow
    right = lookupKey keys RightArrow
    up = lookupKey keys UpArrow
    down = lookupKey keys DownArrow

generateInitState :: IO State
generateInitState = generateRooms >>= \rs -> return initState { playState = (playState initState) { rooms = rs } }

generateRooms :: IO (Map RoomIndex Room)
generateRooms = fmap fromList $ flip mapM rooms $ \r -> fmap (r,) $ do
  doors <- randomDoors
  walls <- randomWalls (\loc -> not $ Map.member loc doors)
  enemies <- randomEnemies (\loc -> not $ Map.member loc doors || S.member loc walls)
  return $ Room doors walls enemies
  where
    rooms = [0..9]

randomDoors :: IO (Map (Int,Int) Door)
randomDoors = fromList <$> (sequence $ replicate 3 $ (,) <$> randomLocation <*> (Door <$> randomRIO roomRange))
  where
    roomRange = (0,9)

randomLocation :: IO (Int, Int)
randomLocation = (,) <$> randomRIO (0,pred gameW) <*> randomRIO (0,pred gameH)

randomWalls :: ((Int, Int) -> Bool) -> IO (S.Set (Int, Int))
randomWalls predicate = S.fromList <$> sequence (replicate ((gameW * gameH) `div` 8) (eligibleLocation predicate))

randomEnemies :: ((Int, Int) -> Bool) -> IO (Map (Int, Int) Enemy)
randomEnemies predicate = fromList <$> sequence [randomEnemy, randomEnemy, randomEnemy]
  where
    randomEnemy = (,) <$> eligibleLocation predicate <*> (Enemy <$> randomRIO (1,3))

eligibleLocation :: ((Int, Int) -> Bool) -> IO (Int, Int)
eligibleLocation predicate = do
  loc <- randomLocation
  if predicate loc
    then return loc
    else eligibleLocation predicate

