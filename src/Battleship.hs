{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Battleship where

import           Control.Lens
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor
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

position :: Prism' (Int, Int) Position
position = prism' (\(Position xy) -> xy) $ \(x,y) -> do
  True <- Just (0 <= x && x < 10 && 0 <= y && y < 10)
  return (Position (x,y))

-- Distance to the nearest occupied square, looking right and down
newtype Distance
  = Distance (Int, Int)
    deriving (Eq, Show)

data Orientation
  = Right
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
    deriving (Eq, Show)

shipSize :: Ship -> (Int, Int)
shipSize Carrier    = (1, 5)
shipSize Battleship = (1, 4)
shipSize Submarine  = (1, 3)
shipSize Cruiser    = (1, 2)
shipSize Patrol     = (1, 1)

-- BOARD
--

data Board a
  = Board [(Ship, Orientation, Position)] (V.Vector a)

instance Functor Board where
  fmap f (Board ships xs) =
    Board ships (fmap f xs)

emptyBoard :: a -> Board a
emptyBoard x = Board [] (V.replicate 100 x)

placeShip :: Ship -> Orientation -> Position
          -> Board Distance -> Maybe (Board Distance)
placeShip = undefined

boardFilled :: Board a -> Bool
boardFilled = undefined

-- RUNNING A GAME
--

data Player
  = P1
  | P2
    deriving (Eq, Show)

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

data GameState
  = GameState{ _statePlayer1 :: (Player, Board Distance)
             , _statePlayer2 :: (Player, Board Distance)
             }
makeLenses ''GameState

runBattleship :: Monad m
              => (Board Distance, Player1 m)
              -> (Board Distance, Player2 m)
              -> P.Producer Move m GameResult
runBattleship (p1Board, p1') (p2Board, p2') =
  withPreparedBoards p1Board p2Board $ \b1 b2 ->
  P.hoist (flip evalStateT undefined) $ do
    -- Kick off the two players
    (pos1, p1) <- lift (lift p1')
    let move0 = checkHit b2 P1 pos1
    P.yield move0

    (pos2, p2) <- lift (lift (p2' move0))
    let move1 = checkHit b1 P2 pos2
    P.yield move1

    let loop :: Monad m
             => Move -> Move
             -> (Player, Board Distance, PlayerM (StateT GameState m))
             -> (Player, Board Distance, PlayerM (StateT GameState m))
             -> P.Producer Move (StateT GameState m) GameResult
        loop movex movey (px, bx, ResumptionT mx) (py, by, my) = do
          (posx, mx') <- lift (mx (movex, movey))
          let move = checkHit by px posx
          P.yield move
          hasWon <- lift (zoom statePlayer1 checkWon)
          if hasWon then return (Won px)
            else loop move movex (py, by, my) (px, bx, mx')

    loop move0 move1 (P1, b1, (P.hoist lift p1)) (P2, b2, (P.hoist lift p2))

withPreparedBoards :: Applicative m
                   => Board Distance -> Board Distance
                   -> (Board Distance -> Board Distance -> m GameResult)
                   -> m GameResult
withPreparedBoards b1 b2 k
  | boardFilled b1 && boardFilled b2 = k b1 b2
  | boardFilled b1 = pure (UnfilledBoard P2)
  | otherwise      = pure (UnfilledBoard P1)

checkHit :: Board Distance -> Player -> Position -> Move
checkHit board player pos =
  Hit player pos

checkWon :: Monad m => StateT (Board Distance) m Bool
checkWon = return True
