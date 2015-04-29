{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Battleship where

import           Control.Comonad
import           Control.Lens
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.List                 (intercalate, sort)
import qualified Data.Vector               as V
import qualified Pipes                     as P
import qualified Pipes.Core                as P
import qualified Pipes.Internal            as P

-- RESUMPTION
--

data ResumptionT r m a
  = ResumptionT{ resumeT :: r -> m (a, ResumptionT r m a) }

instance Functor m => Functor (ResumptionT r m) where
  fmap f (ResumptionT go) =
    ResumptionT $ fmap (bimap f (fmap f)) . go

instance Applicative m => Applicative (ResumptionT r m) where
  pure x = ResumptionT $ \_ -> pure (x, pure x)
  ResumptionT fm <*> ResumptionT xm = ResumptionT $ \r ->
    (\(f, fk) (x, xk) -> (f x, fk <*> xk)) <$> fm r <*> xm r

instance P.MFunctor (ResumptionT r) where
  hoist f (ResumptionT m) =
    ResumptionT $ f . fmap (second (P.hoist f)) . m

type Resumption r
  = ResumptionT r Identity

resume :: Resumption r a -> r -> (a, Resumption r a)
resume = (runIdentity .) . resumeT

fromServer :: forall m b a . Monad m
           => (forall r. P.Server b a m r) -> ResumptionT b m a
fromServer = go
  where
    go :: P.Server b a m P.X -> ResumptionT b m a
    go svr = case svr of
      P.Request _x _ -> error "impossible - x has type X"
      P.Respond x k  -> ResumptionT $ \r -> return (x, go (k r))
      P.M m          -> ResumptionT $ \r -> m >>= flip resumeT r . go
      P.Pure _x      -> error "impossible - x has type X"

-- SIMPLE TYPES
--

newtype Position
  = Position (Int, Int)
    deriving (Eq, Show)

-- Hiding the constructor and only exposing a prism ensures validity.
_Position :: Prism' (Int, Int) Position
_Position = prism' (\(Position xy) -> xy) $ \(x,y) -> do
  True <- Just (0 <= x && x < 10 && 0 <= y && y < 10)
  return (Position (x,y))

-- Distance to the nearest occupied square, looking right and down
newtype Distance
  = Distance (Int, Int)
    deriving (Eq, Show)

_Distance :: Prism' (Int, Int) Distance
_Distance = prism' (\(Distance xy) -> xy) $ \(x,y) -> do
  True <- Just (0 <= x && x < 10 && 0 <= y && y < 10)
  return (Distance (x,y))

data Orientation
  = Across
  | Down
    deriving (Eq, Show)

-- SHIPS
--

data Ship
  = Carrier
  | Battleship
  | Submarine
  | Cruiser
  | Patrol
    deriving (Eq, Ord, Show)

shipSize :: Ship -> (Int, Int)
shipSize Carrier    = (1, 5)
shipSize Battleship = (1, 4)
shipSize Submarine  = (1, 3)
shipSize Cruiser    = (1, 2)
shipSize Patrol     = (1, 1)

-- BOARD
--

data Board a
  = Board [(Ship, Orientation, Position)] Position (V.Vector a)

instance Show a => Show (Board a) where
  show (Board ships pos grid)
    = unlines
    . map (intercalate " " . V.toList . V.map show)
    $ map (\x -> V.slice (x*10) 10 grid) [0..9]

instance Functor Board where
  fmap f (Board ships pos xs) =
    Board ships pos (fmap f xs)

instance Comonad Board where
  extract (Board _ (Position (x,y)) grid) =
    grid V.! (x + (y*10))
  extend f (Board ships pos grid) =
    Board ships pos $
      V.imap (\i _ -> f (Board ships (Position (i `mod` 10, i `div` 10)) grid))
             grid

emptyBoard :: Board Distance
emptyBoard = Board [] (Position (0,0)) $ V.generate 100 go
  where
    go i = Distance (10 - (i `mod` 10), 10 - (i `div` 10))

placeShip :: Ship -> Orientation
          -> Board Distance -> Maybe (Board Distance)
placeShip ship dir b@(Board ships pos grid) = case dir of
  Across -> do
    let Distance (x,y) = extract b
        (shipWidth, shipLength) = shipSize ship
    if x < shipLength || y < shipWidth then Nothing
      else do
        Just $ b =>> (updateAbove pos shipLength) =>> (updateLeft pos shipWidth)
  Down -> undefined

updateAbove :: Position -> Int -> Board Distance -> Distance
updateAbove (Position (newx, newy)) n board =
  if x < newx || x >= newx + n || newy < y then dist
    else Distance (dx, min (newy - y) dy)
  where
    dist@(Distance (dx,dy)) = extract board
    Position (x,y) = boardPosition board

updateLeft :: Position -> Int -> Board Distance -> Distance
updateLeft (Position (newx, newy)) n board =
  if y < newy || y >= newy + n || newx < x then dist
    else Distance (min (newx - x) dx, dy)
  where
    dist@(Distance (dx,dy)) = extract board
    Position (x,y) = boardPosition board

boardPosition :: Board a -> Position
boardPosition (Board _ pos _) = pos

moveRight :: Int -> Board a -> Maybe (Board a)
moveRight n (Board ships (Position (x,y)) grid) =
  if n+x < 10 then Just (Board ships (Position (n+x, y)) grid) else Nothing

moveDown :: Int -> Board a -> Maybe (Board a)
moveDown n (Board ships (Position (x,y)) grid) =
  if n+y < 10 then Just (Board ships (Position (x, n+y)) grid) else Nothing

moveTo :: Position -> Board a -> Board a
moveTo pos (Board ships _ grid) = Board ships pos grid

boardFilled :: Board a -> Bool
boardFilled (Board ships _ _) =
  allShips == [Carrier, Battleship, Submarine, Cruiser, Patrol]
  where
    allShips = sort (ships ^.. traverse._1)

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
    deriving (Eq, Show)

data GameResult
  = Won Player
  | UnfilledBoard Player
    deriving (Eq, Show)

type PlayerM m = ResumptionT (Move, Move) m Position

-- Player 1 must make their first move without information of previous moves.
type Player1 m =         m (Position, PlayerM m)

-- Player 2 sees what move Player 1 made in their first move.
type Player2 m = Move -> m (Position, PlayerM m)

data PlayerState
  = PlayerState{ _playerId    :: Player
               , _playerBoard :: Board Distance
               }
makeLenses ''PlayerState

data GameState
  = GameState{ _gamePlayer1 :: PlayerState
             , _gamePlayer2 :: PlayerState
             }
makeLenses ''GameState

runBattleship :: forall m. Monad m
              => (Board Distance, Player1 m)
              -> (Board Distance, Player2 m)
              -> P.Producer Move m GameResult
runBattleship (p1Board, p1') (p2Board, p2') =
  withPreparedBoards p1Board p2Board $ \st0 ->
  P.hoist (flip evalStateT st0) $ do
    -- Kick off player 1
    (pos1, p1) <- lift (lift p1')
    move0 <- lift . zoom gamePlayer1 $ checkHit pos1
    P.yield move0

    -- ...and player 2
    (pos2, p2) <- lift (lift (p2' move0))
    move1 <- lift . zoom gamePlayer2 $ checkHit pos2
    P.yield move1

    -- Loop between the two players, using two lenses as pointers to switch
    -- between them.
    let loop (movex, lensx, ResumptionT mx) (movey, lensy, my) = do
          (posx, mx') <- lift (mx (movex, movey))
          move <- lift . zoom (cloneLens lensx) $ checkHit posx
          P.yield move
          hasWon <- lift $ zoom (cloneLens lensx) checkWon
          case hasWon of
            Just result -> return result
            Nothing     -> loop (move, lensy, my) (movex, lensx, mx')

    loop (move0, gamePlayer1, P.hoist lift p1)
         (move1, gamePlayer2, P.hoist lift p2)

withPreparedBoards :: Applicative m
                   => Board Distance -> Board Distance
                   -> (GameState -> m GameResult)
                   -> m GameResult
withPreparedBoards b1 b2 k
  | boardFilled b1 && boardFilled b2 = k (initGameState b1 b2)
  | boardFilled b1 = pure (UnfilledBoard P2)
  | otherwise      = pure (UnfilledBoard P1)

initGameState :: Board Distance -> Board Distance -> GameState
initGameState b1 b2 =
  GameState (PlayerState P1 b1) (PlayerState P2 b2)

checkHit :: Monad m => Position -> StateT PlayerState m Move
checkHit pos = do
  p <- use playerId
  return (Hit (otherPlayer p) pos)

checkWon :: Monad m => StateT PlayerState m (Maybe GameResult)
checkWon = do
  return Nothing
