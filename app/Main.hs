{-# LANGUAGE TypeFamilies, NoFieldSelectors, DuplicateRecordFields, DerivingVia, OverloadedStrings #-}

module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.AdditiveGroup (AdditiveGroup (..))
import Data.VectorSpace (VectorSpace (..))
import qualified Data.Text as Text
import Data.Text ( Text )
import Control.Monad (void)
import Text.Printf (printf)
import Data.List (intercalate)

main :: IO ()
main = do
    let toBeCrafted = inventory [(AmmoRifle, 27), (AmmoPistol, 20)]
    cost <- interpret (craftingCost toBeCrafted)
    print cost
    secondaryCost <- interpret (craftingCost cost)
    print secondaryCost

inventory :: [(Item, Rational)] -> Inventory
inventory = Inventory . Map.fromList

craftingCost :: Inventory -> Exp Inventory
craftingCost = Ingredients

interpret :: Exp a -> IO a
interpret (Pure a) = pure a
interpret (Bind x f) = interpret x >>= interpret . f
interpret (Print a) = void $ print a
interpret (Ingredients (Inventory i)) = pure
    $ mconcat
    $ [(n / count item) *^ recipe item | (item, n) <- Map.toList i]

recipe :: Item -> Inventory
recipe item = Inventory $ Map.fromList $ case item of
    AmmoRifle   -> [(MetalFragments, 10), (Gunpowder, 5)]
    AmmoPistol  -> [(MetalFragments, 10), (Gunpowder, 5)]
    Gunpowder   -> [(Charcoal, 30), (Sulfur, 20)]
    _           -> [(item, 1)]

count :: Item -> Rational
count item = case item of
    AmmoRifle   -> 3
    AmmoPistol  -> 4
    Gunpowder   -> 10
    _           -> 1

data Item
    = AmmoRifle
    | AmmoPistol
    | MetalFragments
    | Gunpowder
    | Sulfur
    | Charcoal
    deriving (Eq, Ord)

instance Show Item where
    show = Text.unpack . shortName

shortName :: Item -> Text
shortName item = case item of
    AmmoRifle       -> "ammo.rifle"
    AmmoPistol      -> "ammo.pistol"
    MetalFragments  -> "metal.fragments"
    Gunpowder       -> "gunpowder"
    Sulfur          -> "sulfur"
    Charcoal        -> "charcoal"

newtype Inventory = Inventory { contents :: Map Item Rational }

instance Show Inventory where
    show (Inventory i) = "{" ++ intercalate ", "
        [ printf "\"%s\": %.2f" (shortName item) (fromRational n :: Double)
        | (item, n) <- Map.toList i
        ] ++ "}"

instance AdditiveGroup Inventory where
    zeroV = Inventory mempty
    Inventory a ^+^ Inventory b = Inventory $ Map.unionWith (^+^) a b
    negateV (Inventory a) = Inventory $ negate <$> a

instance VectorSpace Inventory where
    type Scalar Inventory = Rational
    s *^ (Inventory a) = Inventory $ (s *^) <$> a

instance Semigroup Inventory where
    (<>) = (^+^)

instance Monoid Inventory where
    mempty = zeroV

data Exp a where
    Pure :: a -> Exp a
    Bind :: Exp a -> (a -> Exp b) -> Exp b
    Ingredients :: Inventory -> Exp Inventory
    Print :: Show a => a -> Exp ()

instance Functor Exp where
    fmap :: (a -> b) -> Exp a -> Exp b
    fmap f x  = Bind x (Pure . f)

instance Applicative Exp where
    pure = Pure
    f <*> x = Bind f $ \g -> g <$> x

instance Monad Exp where
    (>>=) = Bind