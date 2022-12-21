{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}

module Main where


import Data.List ( subsequences ) 
import Data.SBV

import Control.Lens ( (^?), element )
import Control.Monad ( replicateM )

import GHC.Generics (Generic)
import System.Random (Uniform, Finite)
import System.Random.Stateful (
    uniformM
  , globalStdGen )


data Drink = Milk | Coffee | OrangeJuice
  deriving (Show, Eq, Enum, Bounded, Generic, Finite, Uniform)
data Dish = Salad | Sandwich | Fish
  deriving (Show, Eq, Enum, Bounded, Generic, Finite, Uniform)
data Dessert = Watermelon | Icecream | FruitPie
  deriving (Show, Eq, Enum, Bounded, Generic, Finite, Uniform)

data Menu = Drink Drink | Dish Dish | Dessert Dessert
  deriving (Show)

data Who =
    Me
  | Other
  | Neighbiors
  | Leftside
  | Rightside
  | Everybody
  | Edge
  deriving (Show, Eq, Enum)

type Index = Int

data Target =
    Set Index Who
  | SelectCount Target Int
  | SelectLike Target Menu
  | SelectHate Target Menu
  deriving Show

data Rule =
    Like Target Menu
  | Hate Target Menu
  | SameLikeMenuType Target Menu
  | DiffLikeMenuType Target Menu
  | SameHateMenuType Target Menu
  | DiffHateMenuType Target Menu
  deriving Show

data Person = Person Drink Dish Dessert
  deriving (Show, Generic, Finite, Uniform)


allPerson :: [Person]
allPerson =
  [ Person drink dish dessert
  | drink   <- [ Milk .. ]
  , dish    <- [ Salad .. ]
  , dessert <- [ Watermelon .. ] ]

hasMenu :: Maybe Person -> Menu -> Bool
hasMenu Nothing _                       = False
hasMenu (Just (Person d _ _)) (Drink f)   = d == f
hasMenu (Just (Person _ d _)) (Dish f)    = d == f
hasMenu (Just (Person _ _ d)) (Dessert f) = d == f

randomPerson :: IO Person
randomPerson = uniformM globalStdGen

allPeopleSet :: Int -> [[Person]]
allPeopleSet count = replicateM count allPerson

setTarget :: [Person] -> [Target]
setTarget people =
  [ Set index who
  | index <- [0..length people - 1]
  , who   <- getWho index ]
  where
    getWho :: Index -> [Who]
    getWho index
      | index == 0                 = [ Me, Other, Rightside, Everybody, Edge ]
      | index == length people - 1 = [ Me, Other, Leftside, Everybody, Edge ]
      | otherwise                  = [ Me .. ]

conditionTarget :: [Person] -> [Target] -> [Target]
conditionTarget _ []           = []
conditionTarget people targets =
  let addConditionTargets = concatMap (\target -> map (target, ) $ addCondition people target) targets in
    targets ++ map snd 
      (filter 
        (\(target, ctarget) -> selectTarget people target /= selectTarget people ctarget)
        addConditionTargets)

addCondition :: [Person] -> Target -> [Target]
addCondition people target = let selected = head $ selectTarget people target
                                 count    = length selected - 1 in
     [ SelectCount target index | index <- [1..count-1] ]
  ++ [ SelectLike target menu
  | drink   <- [ Milk .. ]
  , dish    <- [ Salad .. ]
  , dessert <- [ Watermelon .. ]
  , menu    <- [ Drink drink, Dish dish, Dessert dessert ] ]
  ++ [ SelectHate target menu
  | drink   <- [ Milk .. ]
  , dish    <- [ Salad .. ]
  , dessert <- [ Watermelon .. ]
  , menu    <- [ Drink drink, Dish dish, Dessert dessert ] ]


selectTarget :: [Person] -> Target -> [[Int]]
selectTarget people (Set which who) =
  let count = length people - 1 in
  [ case who of
    Me         -> [which]
    Other      -> filter (/=which) [0..count]
    Neighbiors -> filter (\x -> x >= 0 && x <= count) [which-1, which+1]
    Leftside   -> [0..which-1]
    Rightside  -> [which+1..count]
    Everybody  -> [0..count]
    Edge       -> [0, count] ]
selectTarget people (SelectCount target count) =
  filter (/=[])
    [ t | x <- selectTarget people target
        , t <- filter ((count==) . length) $ subsequences x ]
selectTarget people (SelectLike target menu) =
    filter (/=[])
  . map (filter (\x -> hasMenu (people ^? element x) menu))
  $ selectTarget people target
selectTarget people (SelectHate target menu) =
    filter (/=[])
  . map (filter (\x -> not $ hasMenu (people ^? element x) menu))
  $ selectTarget people target


-- selectTarget' :: Target -> [Person] -> [[Person]]



allRule :: [Person] -> [Rule]
allRule people = concatMap getRules [0..length people-1]
  where
    getRules :: Index -> [Rule]
    getRules index =
      [ rules | 
          -- todo : multiple conditions (currently, having only one condition)
          target <- (conditionTarget people . filter (\(Set i _) -> i == index)) (setTarget people)
        , menu   <- map Drink   [ Milk .. ]
                 ++ map Dish    [ Salad .. ]
                 ++ map Dessert [ Watermelon .. ]
        , rules  <- [
            Like target menu,
            Hate target menu ] ]
  

generatePeople :: Int -> IO [Person]
generatePeople count = replicateM count randomPerson

createGame :: Int -> IO ([Person], [Rule])
createGame count = do
  people <- generatePeople count
  let rules = allRule people
  return (people, rules)

main :: IO ()
main = putStrLn "Hello, Haskell!"
