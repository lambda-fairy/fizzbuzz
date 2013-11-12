{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fcontext-stack=1000 #-}

-- | Type-level (compile-time) computation using type families.
--
-- See also: the functional dependencies version at <http://blog.omega-prime.co.uk/?p=28>.
--
-- This takes almost 15 minutes to /compile/ on my laptop.  Be warned.
--
module TypeFamilies where

data Proxy a = Proxy

data Z
data S a

class Reify a where
    reify :: Proxy a -> Integer

instance Reify Z where
    reify _ = 0

instance forall a. Reify a => Reify (S a) where
    reify _ = 1 + reify (Proxy :: Proxy a)

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7
type N9 = S N8

type N10 = Add N5 N5
type N20 = Add N10 N10
type N40 = Add N20 N20
type N50 = Add N40 N10
type N100 = Add N50 N50

data False
data True

data a ::: b
infixr 5 :::

type family IsZero a :: *
type instance IsZero Z = True
type instance IsZero (S a) = False

type family If a t f :: *
type instance If False t f = f
type instance If True t f = t

type family Add a b :: *
type instance Add Z Z = Z
type instance Add Z (S a) = S a
type instance Add (S a) Z = S a
type instance Add (S a) (S b) = S (S (Add a b))

type family Sub a b :: *
type instance Sub a Z = a
type instance Sub (S a) (S b) = Sub a b

type Mod a b = Sub b (Mod' a b Z)
type family Mod' a b c :: *
type instance Mod' Z (S b) Z = S b
type instance Mod' Z (S b) (S c) = S c
type instance Mod' (S a) (S b) Z = Mod' a (S b) b
type instance Mod' (S a) (S b) (S c) = Mod' a (S b) c

type Divides a b = IsZero (Mod a b)

data Fizz
data Buzz
data FizzBuzz

type Fizzify a = Fizzify' (Divides a N3) (Divides a N5) a
type family Fizzify' n3 n5 a :: *
type instance Fizzify' True True a = FizzBuzz
type instance Fizzify' True False a = Fizz
type instance Fizzify' False True a = Buzz
type instance Fizzify' False False a = a

type FBList lim = FBList' lim N1
type family FBList' down up :: *
type instance FBList' Z up = ()
type instance FBList' (S down) up = Fizzify up ::: FBList' down (S up)

class Display a where
    display :: Proxy a -> String

instance Display () where
    display _ = ""

instance Display Z where
    display p = show (reify p)

instance Reify (S a) => Display (S a) where
    display p = show (reify p)

instance forall a b. (Display a, Display b) => Display (a ::: b) where
    display _ = display (Proxy :: Proxy a) ++ "\n" ++ display (Proxy :: Proxy b)

instance Display Fizz where
    display _ = "Fizz"

instance Display Buzz where
    display _ = "Buzz"

instance Display FizzBuzz where
    display _ = "FizzBuzz"

main :: IO ()
main = putStr $ display (Proxy :: Proxy (FBList N100))
