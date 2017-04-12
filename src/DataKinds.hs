{-# LANGUAGE KindSignatures, GADTs, DataKinds #-}

module DataKinds where

data Zero
data Succ n

data Vec :: * -> * -> * where
	Nil :: Vec a Zero
	Cons :: x -> Vec x a -> Vec x (Succ a)
         -- this is we want to catch at compiler time in types of types level : kinds.
	Bad :: Vec Int Char

a = Nil
b = Cons 1 a
-- c = Cons False b -- gives compiler error :)
-- c = Vec 0 0


data Nat = Ze | Su Nat
data NVec :: * -> Nat -> * where
	NNil :: NVec a Ze
	NCons :: x -> NVec x a -> NVec x (Su a)
	-- NBad :: NVec Int Char -- will not compile 
	Good :: NVec Char Ze

