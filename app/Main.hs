{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Monad (replicateM)
import GHC.Generics (Generic)
import System.Random (Uniform, Finite)
import System.Random.Stateful (
    uniformM
  , globalStdGen )


data Drink = Milk | Coffee | OrangeJuice
  deriving (Show, Enum, Bounded, Generic, Finite, Uniform)
data Dish = Salad | Sandwich | Fish
  deriving (Show, Enum, Bounded, Generic, Finite, Uniform)
data Dessert = Watermelon | Icecream | FruitPie
  deriving (Show, Enum, Bounded, Generic, Finite, Uniform)

data Menu = Drink Drink | Dish Dish | Dessert Dessert

data Who =
    Me
  | Other
  | Neighbiors
  | Leftside
  | Rightside
  | Everybody
  | Edge
  deriving (Show, Eq)

type Index = Int

data Target =
    Set Index Who
  | SelectCount Target Int
  | SelectLike Target Menu
  | SelectHate Target Menu

data Rule =
    Like Target Menu
  | Hate Target Menu
  | SameLikeMenuType Target Menu
  | DiffLikeMenuType Target Menu
  | SameHateMenuType Target Menu
  | DiffHateMenuType Target Menu

data Person = Person Drink Dish Dessert
  deriving (Show, Generic, Finite, Uniform)

randomPerson :: IO Person
randomPerson = uniformM globalStdGen

allPeopleSet :: Int -> [[Person]]
allPeopleSet size = replicateM size 
  [ Person drink dish dessert 
  | drink   <- [Milk ..]
  , dish    <- [Salad ..]
  , dessert <- [Watermelon ..] ]

{--
allTargets :: Int -> [Target]

--}

{--
allRules :: Int -> [Rule]
allRules count = 
  
--}

selectPeople :: Int -> IO [Person]
selectPeople count = sequence $ repeat randomPerson


{--
createGame :: Int -> IO ([Person], [Rule])
createGame count = do
  people <- selectPeople count

--}

main :: IO ()
main = putStrLn "Hello, Haskell!"
