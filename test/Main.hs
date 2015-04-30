module Main (main) where

import           Battleship

import           Control.Comonad
import           Control.Lens
import           Control.Monad          (foldM, msum)
import           Data.Monoid            (Sum (..))
import           System.Random
import           Test.QuickCheck.Gen    (Gen (MkGen))
import           Test.QuickCheck.Random (QCGen (..))
import qualified Test.Tasty             as T
import qualified Test.Tasty.QuickCheck  as T

main :: IO ()
main = T.defaultMain $ T.testGroup "all" [boardTests]

boardTests :: T.TestTree
boardTests = T.testGroup "Board" [ placeShip_extracts
                                 , placeShip_commutes
                                 , distances_decrease
                                 ]
  where
    tidy = PrettyBoard . moveTo origin

    placeShip_extracts = T.testProperty "placeShip sets its focus" $
      \x@(SomeShip ship _ _) ->
         case someShip x emptyBoard of
           Nothing -> T.label "Nothing" True
           Just board -> T.label "Just" $ Ship ship == extract board

    placeShip_commutes = T.testProperty "placeShip is commutative" $
      \x y ->
         let go a b = someShip a emptyBoard >>= someShip b
         in case (go x y, go y x) of
           (Nothing, Nothing) -> T.label "Nothing" True
           (xy, yx) -> T.cover True 66 "Just" $
                       (tidy <$> xy) T.=== (tidy <$> yx)

    distances_decrease = T.testProperty "distances decrease globally" $
      \xs -> case calcDistances xs of
        Nothing -> T.label "Nothing" True
        Just ds -> T.counterexample (show ds) $
          T.label "Just" $ and (zipWith (<) ds (drop 1 ds))

newtype PrettyBoard = PrettyBoard (Board Cell)
  deriving Eq

instance Show PrettyBoard where
  show (PrettyBoard board) = displayBoard board

genBoard :: T.Gen PrettyBoard
genBoard = MkGen $ \(QCGen gen) _ -> PrettyBoard (fst (randomBoard gen))

genRandom :: Random a => T.Gen a
genRandom = MkGen $ \(QCGen gen) _ -> fst (random gen)

genRandomR :: Random a => (a, a) -> T.Gen a
genRandomR ul = MkGen $ \(QCGen gen) _ -> fst (randomR ul gen)

data SomeShip
  = SomeShip Ship Position Direction
    deriving Show

instance T.Arbitrary SomeShip where
  arbitrary = do
    -- Only generate ships that fit on the board.
    ship <- genRandom
    dir  <- genRandom
    let Just posArgs = case dir of
          Across -> (,) origin <$> (10 - shipLength ship, 9) ^? _Position
          Down   -> (,) origin <$> (9, 10 - shipLength ship) ^? _Position
    pos <- genRandomR posArgs
    return $ SomeShip ship pos dir

someShip :: SomeShip -> Board Cell -> Maybe (Board Cell)
someShip (SomeShip ship pos dir) = placeShip ship dir . moveTo pos

totalDistance :: Board Cell -> Int
totalDistance = (getSum .) . foldMap $ \x -> case x of
  Ship _     -> mempty
  Empty dist -> Sum . uncurry (+) $ _Distance # dist

calcDistances :: [SomeShip] -> Maybe [Int]
calcDistances = fmap fst . foldM go ([totalDistance emptyBoard], emptyBoard)
  where
    -- Skip placements that fail, there are just too many of them.
    go (dists, board) x =
      msum [ someShip x board >>= \b -> return (totalDistance b:dists, b)
           , return (dists, board) ]
