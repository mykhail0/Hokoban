{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
-- import Data.Text hiding (concat, elem, length, map, reverse)
import System.IO

data Coord = C Integer Integer

instance Eq Coord where C x y == C x' y' = x == x' && y == y'

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)

type DrawFun = Integer -> Integer -> Char

type Picture = DrawFun -> DrawFun

blank :: a -> a
blank = id

(&) :: (b -> c) -> (a -> b) -> a -> c
(&) = (.)

charPicture :: Char -> Picture
charPicture c _ 0 0 = c
charPicture _ f x y = f x y

lettering :: String -> Picture
lettering [] = id
lettering (h : t) = charPicture h & (translated 1 0 $ lettering t)

wall, ground, storage, box, storagePlayer, storageBox, startScreenPic :: Picture
wall = charPicture '#'
ground = charPicture ' '
storage = charPicture '.'
box = charPicture '$'
storagePlayer = charPicture '+'
storageBox = charPicture '*'

translated :: Integer -> Integer -> Picture -> Picture
translated dx dy f drawFun x y =
  f (\a b -> drawFun (a + dx) (b + dy)) (x - dx) (y - dy)

startScreenPic =
  let visualise :: [Maze] -> Picture
      visualise l =
        translated (-8) 0 (lettering "Closed")
          & pictureOfBools (mapList isClosed l)
          & translated
            (relativeTrans l)
            0
            (lettering "Sane" & (translated 8 0 $ pictureOfBools $ mapList isSane l))
   in translated 0 (relativeTrans mazes) (lettering "Mazes")
        & visualise mazes
        & (translated 0 (-2) $ lettering "Bad mazes")
        & (translated 0 (-relativeTrans badMazes) $ visualise badMazes)
  where
    relativeTrans :: [Maze] -> Integer
    relativeTrans l = fromIntegral $ listLength l

winningScreen :: Integer -> Picture
winningScreen moves =
  lettering $ "Moves: " ++ show moves

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

standableTile :: Tile -> Bool
standableTile x = x == Ground || x == Storage

data Direction = R | U | L | D deriving (Eq)

player :: Direction -> Picture
player direction = charPicture c
  where
    c :: Char
    c
      | direction == U = '^'
      | direction == D = 'v'
      | direction == L = '<'
      | direction == R = '>'
      | otherwise = undefined

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord U (C x y) = C x $ y + 1
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord D (C x y) = C x $ y - 1

-- Range for drawing a maze.
rng, startScreenXrng, startScreenYrng, termX, termY :: [Integer]
rng = [-10 .. 10]

type MazeDrawer = Coord -> Tile

data Maze = Maze Coord (Coord -> Tile)

mzCoord :: Maze -> Coord
mzCoord (Maze c _) = c

mzMap :: Maze -> (Coord -> Tile)
mzMap (Maze _ map_) = map_

mazes :: [Maze]
mazes = [defaultMaze, lvl1, lvl2, lvl3, lvl4, lvl5, lvl6, open]

badMazes :: [Maze]
badMazes = [dead1, dead2, dead3, dead4, dead5, dead6, dead7, bad1]

maze :: MazeDrawer
maze (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground

defaultMaze,
  lvl1,
  lvl2,
  lvl3,
  lvl4,
  lvl5,
  lvl6,
  dead1,
  dead2,
  dead3,
  dead4,
  dead5,
  dead6,
  dead7,
  bad1,
  open ::
    Maze
defaultMaze = Maze (C 0 1) maze
lvl1 = Maze (C (-3) 0) lvl1Drawer
  where
    lvl1Drawer (C x y)
      | abs x > 4 || abs y > 1 = Blank
      | abs x == 4 || abs y == 1 = Wall
      | x == 3 && y == 0 = Storage
      | x == 0 && y == 0 = Box
      | otherwise = Ground
open = Maze (C (-3) 0) lvl1Drawer
  where
    lvl1Drawer (C x y)
      | abs x > 4 || abs y > 1 = Blank
      | x == -4 && y == 0 = Blank
      | abs x == 4 || abs y == 1 = Wall
      | x == 3 && y == 0 = Storage
      | x == 0 && y == 0 = Box
      | otherwise = Ground
lvl2 = Maze (C (-2) 3) lvl2Drawer
  where
    lvl2Drawer (C x y)
      | abs x > 4 || abs y > 5 || x == -4 || y == -5 = Blank
      | x == -3 || x == 4 || y == -4 || y == 5 = Wall
      | x > 1 && y > 0 = Wall
      | x == -2 && 0 <= y && y < 3 = Wall
      | -1 <= x && x <= 0 && y == 2 = Wall
      | 0 <= x && x <= 1 && y == -1 = Wall
      | -2 <= x && x <= 1 && y == -3 = Wall
      | x == 3 && y == 0 = Wall
      | x == 2 && y == -1 = Storage
      | x == -1 && y == 3 = Box
      | otherwise = Ground
lvl3 = Maze (C (-2) 0) lvl3Drawer
  where
    lvl3Drawer (C x y)
      | abs x > 4 || abs y > 3 || x == -4 = Blank
      | x == -3 || x == 4 || abs y == 3 = Wall
      | x == 3 && abs y < 2 = Storage
      | x == 0 && abs y < 2 = Box
      | otherwise = Ground
lvl4 = Maze (C 2 0) lvl4Drawer
  where
    lvl4Drawer c@(C x y)
      | abs x > 4 || abs y > 3 || x == -4 = Blank
      | x == -3 || x == 4 || abs y == 3 = Wall
      | x == 0 && -1 <= y && y < 3 = Wall
      | x == 3 && -3 < y && y <= 0 = Wall
      | c `elem` [C 1 2, C 2 (-2)] = Storage
      | c `elem` [C (-1) 1, C 2 (-1)] = Box
      | otherwise = Ground
lvl5 = Maze (C (-2) (-1)) lvl5Drawer
  where
    lvl5Drawer c@(C x y)
      | abs x > 4 || abs y > 3 || y == -3 = Blank
      | abs x == 4 || y == 3 || y == -2 = Wall
      | c `elem` [C (-3) 2, C 0 2, C (-2) 0, C 1 0, C 2 0] = Wall
      | c `elem` [C (-3) 1, C (-1) 1, C 0 (-1), C 2 (-1)] = Storage
      | c `elem` [C (-2) 1, C 2 1, C (-1) (-1), C 1 (-1)] = Box
      | otherwise = Ground
lvl6 = Maze (C (-3) 1) lvl6Drawer
  where
    lvl6Drawer c@(C x y)
      | abs x > 4 || abs y > 3 || y == -3 = Blank
      | abs x == 4 || y == 3 || y == -2 = Wall
      | 0 <= x && x <= 1 && y == 0 = Wall
      | 0 <= x && x <= 3 && y == -1 = Wall
      | x == -1 && y == 2 = Wall
      | c `elem` [C 3 2, C 2 0, C 3 0] = Storage
      | c `elem` [C (-2) 1, C 0 1, C (-2) 0] = Box
      | otherwise = Ground
dead1 = Maze (C (-1) 0) drawer
  where
    drawer (C x y)
      | abs x > 3 || abs y > 2 || x == -3 = Blank
      | x == -2 || x == 3 || abs y == 2 = Wall
      | x == 2 && y == 0 = Storage
      | x == 0 && y == -1 = Box
      | otherwise = Ground
dead2 = Maze (C 0 0) drawer
  where
    drawer c@(C x y)
      | abs x > 3 || abs y > 2 = Blank
      | abs x == 3 || abs y == 2 = Wall
      | c `elem` [C 1 1, C 2 1] = Storage
      | c `elem` [C (-1) 1, C 0 1] = Box
      | otherwise = Ground
bad1 = Maze (C 3 1) drawer
  where
    drawer c@(C x y)
      | abs x > 4 || abs y > 3 || x == -4 || y == -3 = Blank
      | x == 4 || x == -3 || y == 3 || y == -2 = Wall
      | c `elem` [C (-1) (-1), C (-2) (-1), C 1 2] = Wall
      | c `elem` [C 1 1, C (-2) 0] = Storage
      | c `elem` [C 0 0, C 0 1, C (-1) 0, C 2 1] = Box
      | otherwise = Ground
dead3 = Maze (C (-1) 0) drawer
  where
    drawer (C x y)
      | abs x > 3 || abs y > 2 || y == -2 = Blank
      | abs x == 3 || y == 2 || y == -1 = Wall
      | x == -2 = Storage
      | x == 0 = Box
      | otherwise = Ground
dead4 = Maze (C 0 1) drawer
  where
    drawer c@(C x y)
      | abs x > 4 || abs y > 4 || x == -4 || y == -4 = Blank
      | x == -3 || x == 4 || y == -3 || y == 4 = Wall
      | x > 1 && y > 0 = Wall
      | c `elem` [C (-1) 0, C 1 (-1), C 2 (-1)] = Wall
      | c `elem` [C (-2) 3, C (-1) (-2), C 0 (-2)] = Storage
      | c `elem` [C (-2) 1, C 0 0, C 1 0] = Box
      | otherwise = Ground
dead5 = Maze (C 2 (-1)) drawer
  where
    drawer c@(C x y)
      | abs x > 4 || x == -4 || abs y > 3 = Blank
      | x == -3 || x == 4 || abs y == 3 = Wall
      | y == -2 = Storage
      | c `elem` [C (-1) 1, C 0 1, C (-1) 0, C 1 0, C 0 (-1), C 1 (-1)] = Box
      | otherwise = Ground
dead6 = Maze (C 0 (-2)) drawer
  where
    drawer c@(C x y)
      | abs x > 5 || abs y > 4 = Blank
      | abs x == 5 || abs y == 4 = Wall
      | c `elem` [C (-3) (-2), C (-2) (-3), C 1 1, C 1 2, C 2 1] = Wall
      | x >= 0 && y == -3 = Storage
      | x == -1 && y == -1 = Storage
      | c `elem` ([C i j | (i, j) <- l] ++ [C j i | (i, j) <- l]) = Box
      | otherwise = Ground
      where
        l = [(i, i + 1) | i <- [-2 .. 0]]
dead7 = Maze (C 1 1) drawer
  where
    drawer c@(C x y)
      | abs x > 3 || abs y > 3 || x == -3 || y == -3 = Blank
      | x == -2 || x == 3 || y == -2 || y == 3 = Wall
      | c `elem` [C (-1) (-1), C (-1) 2, C 1 0, C 2 (-1)] = Storage
      | c `elem` [C (-1) 0, C 0 2, C 2 1, C 1 (-1)] = Box
      | otherwise = Ground

removeBoxes :: (Coord -> Tile) -> Coord -> Tile
removeBoxes maze_ = f . maze_ where f c = if c == Box then Ground else c

addBoxes :: [Coord] -> MazeDrawer -> MazeDrawer
addBoxes boxes maze_ c
  | c `elem` boxes = Box
  | otherwise = maze_ c

data State = S
  { stPlayer :: Coord,
    stDir :: Direction,
    stBoxes :: [Coord],
    stMap :: MazeDrawer,
    stXdim :: [Integer],
    stYdim :: [Integer],
    stMove :: Integer
  }

instance Eq State where
  (S pl dir box0 _ xdim ydim move) == (S pl1 dir1 box1 _ xdim1 ydim1 move1) =
    pl
      == pl1
      && dir
        == dir1
      && box0
        == box1
      && xdim
        == xdim1
      && ydim
        == ydim1
      && move
        == move1

neighbourTiles :: MazeDrawer -> Coord -> [Coord]
neighbourTiles maze_ coord =
  filterList
    (\e -> maze_ e /= Wall)
    (coord : [adjacentCoord dir coord | dir <- [R, U, L, D]])

isClosed, isSane :: Maze -> Bool
isClosed (Maze coord maze_) =
  standableTile (maze_ coord)
    && isGraphClosed coord (neighbourTiles maze_) (\e -> maze_ e /= Blank)
isSane (Maze coord maze_) =
  let countReachableTiles :: Tile -> Integer
      countReachableTiles tile =
        listLength $
          filterList (\e -> reachable e coord $ neighbourTiles maze_) $
            tileCoords tile maze_ rng rng
   in countReachableTiles Storage >= countReachableTiles Box

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k) (fromIntegral k) (go 0 xs)
  where
    n = length xs
    k = findK 0 -- k is the integer square of n
    findK i
      | i * i >= n = i
      | otherwise = findK (i + 1)
    go _ [] = blank
    go i (b : bs) =
      translated
        (fromIntegral (i `mod` k))
        (-fromIntegral (i `div` k))
        (pictureOfBool b)
        & go (i + 1) bs

    pictureOfBool True = charPicture 'o'
    pictureOfBool False = charPicture 'x'

isWinning :: State -> Bool
isWinning s =
  allList
    (\c -> maze_ c == Storage)
    [c | c <- (stBoxes s), reachable c (stPlayer s) $ neighbourTiles maze_]
  where
    maze_ = stMap s

pictures :: [Picture] -> Picture
pictures [] = id
pictures (h : t) = h & pictures t

draw :: State -> Picture
draw s@(S playerCoord direction boxes maze_ xdim ydim moves)
  | isWinning s = winningScreen moves
  | otherwise =
      atCoord
        playerCoord
        ( if maze_ playerCoord == Storage
            then storagePlayer
            else playerImg
        )
        & pictures
          [ atCoord c $ if maze c == Storage then storageBox else box
            | c <- boxes
          ]
        & pictures [atCoord (C x y) $ drawTile $ maze_ (C x y) | x <- xdim, y <- ydim]
  where
    playerImg :: Picture
    playerImg = player direction

tileCoords :: Tile -> MazeDrawer -> [Integer] -> [Integer] -> [Coord]
tileCoords tile maze_ xdim ydim =
  [C x y | x <- xdim, y <- ydim, maze_ (C x y) == tile]

initialState :: Direction -> Maze -> [Integer] -> [Integer] -> State
initialState direction (Maze playerCoord mazeMap) xdim ydim =
  S
    playerCoord
    direction
    [ c | c <- (tileCoords Box mazeMap xdim ydim), reachable c playerCoord $ neighbourTiles mazeMap
    ]
    (removeBoxes mazeMap)
    xdim
    ydim
    0

testState :: State
testState = initialState U defaultMaze rng rng

-- TMP
createTestState :: Maze -> State
createTestState mz = initialState U mz rng rng

data Event = KeyPress String

instance Eq Event where KeyPress x == KeyPress y = x == y

handleEvent :: Event -> State -> State
handleEvent k s
  | isWinning s = s
  | isJust maybeDir =
      let direction :: Direction
          direction = fromJust maybeDir
          newCoord :: Coord
          newCoord = adjacentCoord direction $ stPlayer s
          boxes :: [Coord]
          boxes = stBoxes s
          mazeWithBoxes :: MazeDrawer
          mazeWithBoxes = addBoxes boxes $ stMap s
          validMove :: Bool
          validMove =
            standableTile tile
              || ( tile
                     == Box
                     && (standableTile $ mazeWithBoxes $ adjacentCoord direction newCoord)
                 )
            where
              tile :: Tile
              tile = mazeWithBoxes newCoord
          changeBox :: Coord -> Coord
          changeBox c
            | c == newCoord = adjacentCoord direction newCoord
            | otherwise = c
       in if validMove
            then
              s
                { stPlayer = newCoord,
                  stDir = direction,
                  stBoxes = map changeBox boxes,
                  stMove = 1 + stMove s
                }
            else s
  | otherwise = s
  where
    dir :: Event -> Maybe Direction
    dir (KeyPress key)
      | key == "d" = Just R
      | key == "w" = Just U
      | key == "a" = Just L
      | key == "s" = Just D
      | otherwise = Nothing
    maybeDir :: Maybe Direction
    maybeDir = dir k

data SSState world = StartScreen | Running world

type Screen = String

data Activity world
  = Activity
      world
      (Event -> world -> world)
      (world -> Screen)

data WithUndo a = WithUndo a [a]

withUndo :: (Eq a) => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw_) = Activity state0' handle' draw'
  where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "u" =
      case stack of
        s' : stack' -> WithUndo s' stack'
        [] -> WithUndo s []
    handle' e (WithUndo s stack)
      | s' == s = WithUndo s stack
      | otherwise = WithUndo (handle e s) (s : stack)
      where
        s' = handle e s
    draw' (WithUndo s _) = draw_ s

resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw_) =
  Activity state0 handle' draw_
  where
    handle' (KeyPress key) _ | key == "\ESC" = state0
    handle' e s = handle e s

startScreenXrng = [-15 .. 15]

startScreenYrng = reverse [-10 .. 10]

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw_) =
  Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
      | key == " " = Running state0
    handle' _ StartScreen = StartScreen
    handle' e (Running s) = Running (handle e s)

    draw' StartScreen = createScreen startScreenXrng startScreenYrng startScreenPic
    draw' (Running s) = draw_ s

data WithLevels a = Levels
  { states :: [a],
    handles :: [(Event -> a -> a)],
    drawers :: [(a -> Screen)]
  }

withLevels :: [Activity s] -> Activity (WithLevels s)
withLevels [] = undefined
withLevels [Activity state0 handle draw_] = Activity state0' handle' draw'
  where
    state0' = Levels [state0] [handle] [draw_]
    handle' :: Event -> (WithLevels s) -> (WithLevels s)
    handle'
      (KeyPress key)
      (Levels (_ : state1 : states_) (_ : handle1 : handles_) (_ : draw1 : drawers_))
        | key == "n" = Levels (state1 : states_) (handle1 : handles_) (draw1 : drawers_)
    handle' e (Levels (state1 : states_) (handle1 : handles_) drawers_) =
      Levels ((handle1 e state1) : states_) (handle1 : handles_) drawers_
    handle' _ _ = undefined
    draw' (Levels (state1 : _) _ (draw1 : _)) = draw1 state1
    draw' _ = undefined
withLevels ((Activity state0 handle draw_) : b : t) = Activity state0' handle' draw'
  where
    (Activity (Levels states_ handles_ drawers_) handle' draw') = withLevels (b : t)
    state0' = Levels (state0 : states_) (handle : handles_) (draw_ : drawers_)

activityOf :: world -> (Event -> world -> world) -> (world -> Screen) -> IO ()
activityOf state0 handler drawer =
  let go state = do
        c <- getChar
        let nextState = handler (KeyPress [c]) state
        putStr "\ESCc"
        putStr $ drawer nextState
        go nextState
   in do
        hSetBuffering stdin NoBuffering
        putStr "\ESCc"
        putStr $ drawer state0
        go state0

runActivity :: Activity s -> IO ()
runActivity (Activity state handle draw_) = activityOf state handle draw_

termX = [-10 .. 10]

termY = reverse [-10 .. 10]

basicActivity :: Maze -> Activity State
basicActivity maze_ =
  Activity
    (createTestState maze_)
    handleEvent
    (createScreen termX termY . draw)

main :: IO ()
main =
  runActivity $
    withStartScreen $
      withLevels
        [resettable $ withUndo $ basicActivity maze_ | maze_ <- mazes]

createScreen :: [Integer] -> [Integer] -> Picture -> String
createScreen _ [] _ = []
createScreen xdim ydim p =
  concat [[p (\_ _ -> ' ') x y | x <- xdim] ++ ['\n'] | y <- ydim]

-- General auxiliary functions.
elemList :: (Eq a) => a -> [a] -> Bool
elemList _ [] = False
elemList e (h : t)
  | e == h = True
  | otherwise = elemList e t

-- elemList e l = foldList (\x b -> if b then True else x == e) False l

appendList :: [a] -> [a] -> [a]
appendList [] l = l
appendList (h : t) l = h : (appendList t l)

-- appendList a b = foldList (:) b $ reverse a

listLength :: [a] -> Integer
listLength [] = 0
listLength (_ : t) = 1 + listLength t

-- listLength l = foldList (\_ i -> i + 1) 0 $ l

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList f (h : t)
  | f h = h : l
  | otherwise = l
  where
    l = filterList f t

-- filterList f l = reverse $ foldList (\e lst -> if f e then e:lst else lst) [] l

nth :: [a] -> Integer -> a
nth [] _ = undefined
nth (h : _) 0 = h
nth (_ : t) i = nth t $ i - 1

-- nth (h:t) i

-- | b = e
-- | otherwise = nth [] 0
-- where
--  (e, _, b) = foldList
--   (\e (x, j, b) -> if j == i then (e, j + 1, True) else (x, j + 1, b))
--   (h, 1, i == 0) t
mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (h : t) = (f h) : (mapList f t)

-- mapList f l = reverse $ foldList (\e lst -> (f e):lst) [] l

andList :: [Bool] -> Bool
andList l = not $ elemList False l

-- andList l = foldList (\e acc -> if acc then e else False) True l

allList :: (a -> Bool) -> [a] -> Bool
allList _ [] = True
allList f (h : t)
  | f h = allList f t
  | otherwise = False

-- allList f l = foldList (\e acc -> if acc then f e else False) True l

-- foldl
foldList :: (a -> b -> b) -> b -> [a] -> b
foldList _ acc [] = acc
foldList f acc (h : t) = foldList f (f h acc) t

notVisitedNeighbours :: (Eq a) => a -> (a -> [a]) -> [a] -> [a]
notVisitedNeighbours v neighbours visited =
  filterList (\e -> not $ elemList e visited) $ neighbours v

isGraphClosed :: (Eq a) => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk =
  let -- First arg is a stack of vertices to visit.
      isGraphClosed_ :: (Eq a) => [a] -> (a -> [a]) -> (a -> Bool) -> [a] -> Bool
      isGraphClosed_ [] _ _ _ = True
      isGraphClosed_ (h : t) neighbours_ isOk_ visited
        | isOk_ h =
            isGraphClosed_ (appendList t notVisited) neighbours_ isOk_ $
              appendList visited notVisited
        | otherwise = False
        where
          notVisited = notVisitedNeighbours h neighbours_ visited
   in isGraphClosed_ neighb neighbours isOk neighb
  where
    neighb = neighbours initial

-- Technically wrong but may be more useful.
-- in isGraphClosed_ [initial] neighbours isOk [initial]

reachable :: (Eq a) => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours =
  let reachable_ :: (Eq a) => a -> [a] -> (a -> [a]) -> [a] -> Bool
      reachable_ _ [] _ _ = False
      reachable_ v_ (h : t) neighbours_ visited
        | v_ == h = True
        | otherwise =
            reachable_ v_ (appendList t notVisited) neighbours_ $
              appendList visited notVisited
        where
          notVisited = notVisitedNeighbours h neighbours_ visited
   in reachable_ v neighb neighbours neighb
  where
    neighb = neighbours initial

-- Technically wrong but may be more useful.
-- in reachable_ v [initial] neighbours [initial]

allReachable :: (Eq a) => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours =
  allList (\e -> reachable e initial neighbours) vs

mulTuple :: Double -> (Double, Double) -> (Double, Double)
mulTuple k (x, y) = (k * x, k * y)
