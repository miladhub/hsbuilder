module Main where

import Lib
import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Builder a b = Builder
  {
    foos :: [a]
  , foo  :: b
  }
  deriving (Eq, Show)

instance Functor (Builder a) where
  fmap f (Builder as b) = Builder as (f b)

instance Semigroup b => Semigroup (Builder a b) where
  (Builder as b) <> (Builder as' b') = Builder (as ++ as') (b <> b')

instance Monoid b => Monoid (Builder a b) where
  mempty = Builder [] mempty

instance Applicative (Builder a) where
  pure = Builder []
  (Builder as fbc) <*> (Builder as' b) = Builder (as <> as') (fbc b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Builder a b) where
  arbitrary = Builder <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Builder a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative $ Builder [1 :: Int, 2 :: Int] ("b", "w", 1 :: Int)

{-
*Main Lib> generate (arbitrary :: Gen (Builder [Int] String))
Builder {foos = [[18,-24,-22,16,29,29,-13,25,-12,27,4,19,-21],[23,-13,5,-16,-16,2,30,-14,3],[27,-26,0,2,-18,-7,26,23,-10,10,-20,-13,-2,-3,5,-1,-27,-3,-12,2,0,-5,-12,24,26,-26,16,-27,1],[-29,26,-4,-14,-11,-10,-22,-28,-10,1,2,12,-19,22,-30,-20,23,-1,23,-14,-30,-29,3,20,-5,-6,20],[11,-28,3,1,-22,-19,-22,-10,-14,18,25,-26,-2,-24,-22]], foo = "o,\386909CF(\EOT&\SYN\186910WUnP]&\RSU:}\SUB2\ESC\426004`bH_"}

*Main Lib> :{
*Main Lib| b
*Main Lib| { foos = [1,2,3] }
*Main Lib| { foos = [4,5,6] }
*Main Lib| :}
Builder {foos = [4,5,6], foo = "foo"}
*Main Lib> pure (++) <*> b <*> b
Builder {foos = [], foo = "foofoo"}
*Main Lib> pure (++) <*> b{ foos = [1,2,3] } <*> b{ foos = [4,5,6] }
Builder {foos = [1,2,3,4,5,6], foo = "foofoo"}

*Main Lib> b = Builder [] ()
*Main Lib> pure id <*> b
Builder {foos = [], foo = ()}
*Main Lib> pure id <*> b { foos = [1,2,3] }
Builder {foos = [1,2,3], foo = ()}
*Main Lib> pure id <*> b { foos = [1,2,3] } <*> b { foos = [4,5,6] }

<interactive>:75:1: error:
...
-}
