{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Battleship
  ( -- * Reactives
    ReactiveT(..), Reactive, react
    -- * Basic Types
  , Position, _Position, origin, Distance, _Distance, Direction(..)
    -- * Game Setup Types
  , Ship(..), allShips, shipLength
  , Cell(..), Board, emptyBoard, randomBoard, boardReady, placeShip
  , boardPosition, moveRight, moveDown, moveTo
  , displayBoard
    -- * Game Types
  , Player(..), otherPlayer
  , Move(..), GameResult(..)
  , Player1, Player2
  , runBattleship
  ) where

import           Control.Comonad           (Comonad (..), (=>>))
import           Control.Lens
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, evalStateT, runState, state)
import           Data.Bifunctor            (first, second)
import           Data.Bool                 (bool)
import           Data.List                 (intercalate, sort)
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import qualified Data.Vector               as V
import           Pipes                     ((>->))
import qualified Pipes                     as P
import qualified Pipes.Prelude             as P
import           System.Random             (Random (..), RandomGen (..),
                                            getStdGen, setStdGen)

-- RESUMPTION
--

-- | An endless process that responds to inputs with a continuation.
data ReactiveT r m a
  = ReactiveT{ reactT :: r -> m (a, ReactiveT r m a) }

instance Functor m => Functor (ReactiveT r m) where
  fmap f (ReactiveT go) =
    ReactiveT $ fmap (bimap f (fmap f)) . go

instance Applicative m => Applicative (ReactiveT r m) where
  pure x = ReactiveT $ \_ -> pure (x, pure x)
  ReactiveT fm <*> ReactiveT xm = ReactiveT $ \r ->
    (\(f, fk) (x, xk) -> (f x, fk <*> xk)) <$> fm r <*> xm r

instance P.MFunctor (ReactiveT r) where
  hoist f (ReactiveT m) =
    ReactiveT $ f . fmap (second (P.hoist f)) . m

type Reactive r
  = ReactiveT r Identity

react :: Reactive r a -> r -> (a, Reactive r a)
react = (runIdentity .) . reactT

-- SIMPLE TYPES
--

newtype Position
  = Position (Int, Int)
    deriving (Eq, Ord, Show)

-- | Hiding the constructor and only exposing a prism ensures positions are
-- sensible.
_Position :: Prism' (Int, Int) Position
_Position = prism' (\(Position xy) -> xy) $ \(x,y) -> do
  True <- Just (0 <= x && x < 10 && 0 <= y && y < 10)
  return (Position (x,y))

origin :: Position
origin = Position (0,0)

instance Random Position where
  randomR (Position (lox,loy), Position (hix,hiy)) g =
    flip runState g $ do
      x <- state $ randomR (lox, hix)
      y <- state $ randomR (loy, hiy)
      return $ Position (x, y)
  random =
    randomR (Position (0,0), Position (9,9))

-- | Distance to the nearest ship, looking right and down
newtype Distance
  = Distance (Int, Int)
    deriving (Eq, Show)

-- | Hiding the constructor and only exposing a prism ensures distances are
-- sensible.
_Distance :: Prism' (Int, Int) Distance
_Distance = prism' (\(Distance xy) -> xy) $ \(x,y) -> do
  True <- Just (0 <= x && x < 10 && 0 <= y && y < 10)
  return (Distance (x,y))

data Direction
  = Across    -- ^ To the right, but that clashes with 'Right'.
  | Down
    deriving (Eq, Show)

instance Random Direction where
  randomR (x,y) g =
    if x == y then (x, g) else random g
  random =
    first (bool Across Down) . random

-- SHIPS
--

data Ship
  = Carrier
  | Battleship
  | Submarine
  | Cruiser
  | Patrol
    deriving (Eq, Ord, Enum, Show)

instance Random Ship where
  randomR (x,y) = first (ships !!) . randomR (0, length ships - 1)
    where
      ships = [min x y .. max x y]
  random = first (allShips !!) . randomR (0, 4)

allShips :: [Ship]
allShips = [Carrier, Battleship, Submarine, Cruiser, Patrol]

-- | The number of cells a ship occupies.
shipLength :: Ship -> Int
shipLength Carrier    = 5
shipLength Battleship = 4
shipLength Submarine  = 3
shipLength Cruiser    = 2
shipLength Patrol     = 1

-- BOARD
--

data Board a
  = Board (Map Ship (Direction, Position)) Position (V.Vector a)
    deriving Eq

instance Show a => Show (Board a) where
  show (Board _ _ grid)
    = unlines
    . map (intercalate " " . V.toList . V.map show)
    $ map (\x -> V.slice (x*10) 10 grid) [0..9]

instance Functor Board where
  fmap f (Board ships pos grid) =
    Board ships pos (fmap f grid)

instance Foldable Board where
  foldMap f (Board _ _ grid) =
    foldMap f grid

instance Comonad Board where
  extract (Board _ (Position (x,y)) grid) =
    grid V.! (x + (y*10))
  extend f (Board ships pos grid) =
    Board ships pos $
      V.generate (V.length grid) $ \i ->
        f (Board ships (Position (i `mod` 10, i `div` 10)) grid)

data Cell
  = Ship Ship      -- ^ The ship type that occupies this cell.
  | Empty Distance -- ^ The distance to the nearest ship to the right and below.
    deriving (Eq, Show)

-- | The empty board.
emptyBoard :: Board Cell
emptyBoard =
  Board M.empty (Position (0,0)) $
    V.generate 100 $ \i ->
      Empty (Distance (10 - (i `mod` 10), 10 - (i `div` 10)))

-- | Put a ship on a board in the current position in a given orientation.
placeShip :: Ship -> Direction -> Board Cell -> Maybe (Board Cell)
placeShip ship dir b@(Board ships pos _) =
  let addShip (Board ships pos' grid) =
        Board (M.insert ship (dir, pos') ships) pos' grid
      len = shipLength ship
  in case (ship `M.member` ships, extract b, dir) of
    (True, _, _) -> Nothing
    (_, Ship _, _) -> Nothing
    (_, Empty (Distance (x,_)), Across) ->
      if x < len then Nothing
        else return . addShip $
               b =>> updateAbove ship pos len =>> updateLeft ship pos 1
    (_, Empty (Distance (_,y)), Down) ->
      if y < shipLength ship then Nothing
        else return . addShip $
               b =>> updateLeft ship pos len =>> updateAbove ship pos 1

-- | Update the distance to the new ship for all cells above it.
updateAbove :: Ship -> Position -> Int -> Board Cell -> Cell
updateAbove ship (Position (newx, newy)) n board =
  case extract board of
    Ship ship' -> Ship ship'
    Empty dist@(Distance (dx, dy)) ->
      if x < newx || x >= newx + n || newy < y then Empty dist
        else if newy == y then Ship ship
               else Empty (Distance (dx, min (newy - y) dy))
  where
    Position (x,y) = boardPosition board

-- | Update the distance to the new ship for all cells to the left of it.
updateLeft :: Ship -> Position -> Int -> Board Cell -> Cell
updateLeft ship (Position (newx, newy)) n board =
  case extract board of
    Ship ship' -> Ship ship'
    Empty dist@(Distance (dx, dy)) ->
      if y < newy || y >= newy + n || newx < x then Empty dist
        else if newx == x then Ship ship
               else Empty (Distance (min (newx - x) dx, dy))
  where
    Position (x,y) = boardPosition board

-- | Get the focus position of a board.
boardPosition :: Board a -> Position
boardPosition (Board _ pos _) = pos

-- | Move the focus to the right n cells.
moveRight :: Int -> Board a -> Maybe (Board a)
moveRight n (Board ships (Position (x,y)) grid) =
  if n+x < 10 then Just (Board ships (Position (n+x, y)) grid) else Nothing

-- | Move the focus down n cells.
moveDown :: Int -> Board a -> Maybe (Board a)
moveDown n (Board ships (Position (x,y)) grid) =
  if n+y < 10 then Just (Board ships (Position (x, n+y)) grid) else Nothing

-- | Move to a specific cell.
moveTo :: Position -> Board a -> Board a
moveTo pos (Board ships _ grid) = Board ships pos grid

-- | Check if a board is filled and ready to play with.
boardReady :: Board a -> Bool
boardReady (Board ships _ _) = sort (M.keys ships) == allShips

-- | Generate a random board.
randomBoard :: RandomGen g => g -> (Board Cell, g)
randomBoard gen = foldr go (emptyBoard, gen) allShips
  where
    go :: RandomGen g => Ship -> (Board Cell, g) -> (Board Cell, g)
    go ship (board, g) =
      let (pos, g')  = random g
          (dir, g'') = random g'
      in maybe (go ship (board, g'')) (\b -> (b, g''))
           (placeShip ship dir (moveTo pos board))

-- RUNNING A GAME
--

data Player
  = P1
  | P2
    deriving (Eq, Show)

otherPlayer :: Player -> Player
otherPlayer P1 = P2
otherPlayer P2 = P1

data Move
  = Hit Player Position
  | Miss Player Position
  | Sunk Player Ship Position
    deriving (Eq, Show)

data GameResult
  = Won Player
  | UnfilledBoard Player
    deriving (Eq, Show)

-- | Player 1 must make their first move without information of previous moves.
-- The 'fst' 'Move' is the result of Player 1's last move and the 'snd' is what
-- Player 2 did.
type Player1 m =         m (Position, ReactiveT (Move, Move) m Position)

-- | Player 2 sees what move Player 1 made in their first move.
-- The 'fst' 'Move' is the result of Player 2's last move and the 'snd' is what
-- Player 1 did.
type Player2 m = Move -> m (Position, ReactiveT (Move, Move) m Position)

data PlayerState
  = PlayerState{ _playerId    :: Player
               , _playerBoard :: Board Cell
               , _playerSunk  :: Map Ship (Set Position)
               }
makeLenses ''PlayerState

data GameState
  = GameState{ _gamePlayer1 :: PlayerState
             , _gamePlayer2 :: PlayerState
             }
makeLenses ''GameState

-- | Run a game of Battleship as a 'P.Producer', spitting out each 'Move' as it
-- is made and finishing the game with a report on who 'Won'.
runBattleship :: Monad m
              => (Board Cell, Player1 m)
              -> (Board Cell, Player2 m)
              -> P.Producer Move m GameResult
runBattleship (p1Board, p1') (p2Board, p2') =
  withPreparedBoards p1Board p2Board $ \st0 ->
  (flip evalStateT st0) $ do
    -- Kick off player 1
    (pos1, p1) <- lift (lift p1')
    move0 <- zoom gamePlayer2 $ checkHit pos1
    lift (P.yield move0)

    -- ...and player 2
    (pos2, p2) <- lift (lift (p2' move0))
    move1 <- zoom gamePlayer1 $ checkHit pos2
    lift (P.yield move1)

    -- Loop between the two players, using two lenses as pointers to switch
    -- between them.
    let loop (movex, lensx, ReactiveT mx) (movey, lensy, my) = do
          (posx, mx') <- lift (mx (movex, movey))
          movex' <- zoom (cloneLens lensy) $ checkHit posx
          lift (P.yield movex')
          hasWon <- zoom (cloneLens lensy) checkWon
          case hasWon of
            Just result -> return result
            Nothing     -> loop (movey, lensy, my) (movex', lensx, mx')

    loop (move0, gamePlayer1, P.hoist lift p1)
         (move1, gamePlayer2, P.hoist lift p2)

withPreparedBoards :: Applicative m
                   => Board Cell -> Board Cell
                   -> (GameState -> m GameResult)
                   -> m GameResult
withPreparedBoards b1 b2 k
  | boardReady b1 && boardReady b2 = k (initGameState b1 b2)
  | boardReady b1 = pure (UnfilledBoard P2)
  | otherwise     = pure (UnfilledBoard P1)

initGameState :: Board Cell -> Board Cell -> GameState
initGameState b1@(Board ships1 _ _) b2@(Board ships2 _ _) =
  GameState (PlayerState P1 b1 hits1) (PlayerState P2 b2 hits2)
  where
    hits1 = initialMap ships1
    hits2 = initialMap ships2
    initialMap ships =
      M.fromList [ (ship, S.fromList positions)
                 | (ship, (dir, pos)) <- M.toList ships
                 , let positions = shipPositions ship dir pos
                 ]

shipPositions :: Ship -> Direction -> Position -> [Position]
shipPositions ship dir (Position (x,y)) =
  [ Position (x', y')
  | n <- [0..(shipLength ship - 1)]
  , let x' = if dir == Across then x + n else x
        y' = if dir == Down   then y + n else y
  , x' < 10 && y' < 10
  ]

checkHit :: Monad m => Position -> StateT PlayerState m Move
checkHit pos = do
  playerBoard %= moveTo pos
  p    <- uses playerId otherPlayer
  here <- uses playerBoard extract
  case here of
    Empty _distance -> return (Miss p pos)
    Ship ship -> do
      remainingM <- use (playerSunk . at ship)
      -- Strange case: if a ship has been sunk or this position has already been
      -- hit, just return a miss.
      case remainingM of
        Nothing -> return (Miss p pos)
        Just remaining ->
          case (remaining == S.singleton pos, pos `S.member` remaining) of
            (True, _) ->
              Sunk p ship pos <$ (playerSunk . at ship .= Nothing)
            (False, True) ->
              Hit p pos <$ (playerSunk . at ship %= fmap (S.delete pos))
            (False, False) ->
              return (Miss p pos)

checkWon :: Monad m => StateT PlayerState m (Maybe GameResult)
checkWon = do
  p <- uses playerId otherPlayer
  allSunk <- uses playerSunk M.null
  if allSunk then return (Just (Won p)) else return Nothing

-- | Handy function for debugging.
displayBoard :: Board Cell -> String
displayBoard board = show . flip fmap board $ \cell -> case cell of
  Ship ship -> shipLength ship
  Empty _ -> 0

test :: IO ()
test = do
  gen <- getStdGen
  let (board1, gen' ) = randomBoard gen
      (board2, gen'') = randomBoard gen'
  setStdGen gen''
  putStrLn (displayBoard board1)
  putStrLn (displayBoard board2)

  let askPos = do
        x <- putStr "X: " *> readLn
        y <- putStr "Y: " *> readLn
        case (x,y) ^? _Position of
          Nothing -> askPos
          Just pos -> return pos

      player1 = do
        let loop = ReactiveT $ \(moveA, moveB) -> do
              putStrLn "\nPlayer 1"
              putStrLn ("Your move: " ++ show moveA)
              putStrLn ("Their move: " ++ show moveB)
              pos <- askPos
              return (pos, loop)
        putStrLn "\nPlayer 1"
        pos <- askPos
        return (pos, loop)

      player2 move0 = do
        let loop = ReactiveT $ \(moveA, moveB) -> do
              putStrLn "\nPlayer 2"
              putStrLn ("Your move: " ++ show moveA)
              putStrLn ("Their move: " ++ show moveB)
              pos <- askPos
              return (pos, loop)
        putStrLn "\nPlayer 2"
        putStrLn ("Their move: " ++ show move0)
        pos <- askPos
        return (pos, loop)

  result <- P.runEffect $
    runBattleship (board1, player1) (board2, player2) >-> P.print
  print result
