{-# LANGUAGE ConstraintKinds,
             DataKinds,
             KindSignatures,
             TypeOperators,
             GADTs,
             TypeFamilies,
             StandaloneDeriving,
             RankNTypes,
             ScopedTypeVariables #-}

module MillerRabin where

import GHC.TypeLits
import Data.Proxy(Proxy(..))
import Control.Arrow
import System.Random -- for now

newtype Mod i (n :: Nat) = Mod i
  deriving (Eq, Ord, Show)

toMod :: forall n i. (Integral i, KnownNat n) => i -> Mod i n
toMod i = Mod $ i `mod` (fromInteger $ natVal (Proxy :: Proxy n))


unMod :: Mod i n -> i
unMod (Mod i) = i

modAdd :: forall i n. (Integral i, KnownNat n) => Mod i n -> Mod i n -> Mod i n
modAdd (Mod i1) (Mod i2) = toMod (i1 + i2)

modNegate :: forall i n. (Integral i, KnownNat n) => Mod i n -> Mod i n
modNegate (Mod i) = toMod ( (fromInteger $ natVal (Proxy :: Proxy n)) - i) 

modMinus :: forall i n. (Integral i, KnownNat n) => Mod i n -> Mod i n -> Mod i n
modMinus = curry $ (second modNegate >>> uncurry modAdd)

modMult :: forall i n. (Integral i , KnownNat n) => Mod i n -> Mod i n -> Mod i n
modMult (Mod i1) (Mod i2) = toMod (i1*i2)

instance (Integral i, KnownNat n) => Num (Mod i n) where
  fromInteger = toMod.fromInteger
  (+) = modAdd
  (-) = modMinus
  (*) = modMult
  negate = modNegate
  abs = id
  signum = const 1

millerRabin ::  Integer -> Integer -> IO Bool
millerRabin 0 _ = return True
millerRabin n p =  do  gen <- getStdGen
                       let (a :: Integer, _) = randomR (1,p-1) gen
                       let Just someNat = someNatVal p
                       case someNat of
                         SomeNat (_ :: Proxy p) -> do
                               let aMod :: Mod Integer p
                                   aMod = toMod p
                               let b1 = millerRabinCheck aMod
                               bRest <- millerRabin (n-1) p
                               return (b1 && bRest)

millerRabinCheck:: forall n. KnownNat n => (Mod Integer n) -> Bool
millerRabinCheck a = (firstCheck == 1) || ((-1) `elem` lsVals)
  where nInteger = natVal (Proxy :: Proxy n)
        (r,d) = decomp (nInteger - 1)
        firstCheck = a^d
        lsVals :: [Mod Integer n] = take (fromInteger r) $ iterate (\n -> n^2) firstCheck


twoAdicVal :: Integer -> Integer
twoAdicVal n = if even n then (1 + twoAdicVal (n `div` 2)) else 0

oddComponent :: Integer -> Integer
oddComponent n = n `div` (2^((twoAdicVal n)))

decomp :: Integer -> (Integer,Integer)
decomp = twoAdicVal &&& oddComponent


