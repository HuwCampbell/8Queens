{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies, KindSignatures,
             TemplateHaskell, GADTs, UndecidableInstances, RankNTypes,
             ScopedTypeVariables, FlexibleContexts, TypeInType #-}

module TypeChess where

import Data.Kind ( Type )
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Data.Singletons.TypeLits

type family Safe (b :: [Nat]) (a :: Nat) :: Bool where
  Safe xs x =
    And '[ All ((:/=$$) x) xs
         , All ((:/=$$) x)           ( ZipWith (:+$) xs                   (EnumFromTo 1 8) )
         , All ((:/=$$) ( 10 :+ x )) ( ZipWith (:-$) (Map ((:+$$) 10) xs) (EnumFromTo 1 8) ) ]

data Safe1 :: [Nat] -> (Nat ~> Bool)
type instance Apply (Safe1 xs) x = Safe xs x

type family Place (a :: [Nat]) (b :: k) :: [[Nat]] where
  Place xs ignore =
    Map (FlipCons xs) (Filter (Safe1 xs) (EnumFromTo 1 8))

data FlipCons :: [Nat] -> (Nat ~> [Nat])
type instance Apply (FlipCons xs) x = x ': xs

data Place1 :: [Nat] -> (ignore ~> [[Nat]])
type instance Apply (Place1 xs) b = Place xs b

data Place2 :: ([Nat] ~> ignore ~> [[Nat]])
type instance Apply (Place2) xs = Place1 xs

type family FoldM ( f :: TyFun b (TyFun a [b] -> Type) -> Type) ( acc :: b ) ( over :: [a] ) :: [b] where
  FoldM f acc '[] = '[ acc ]
  FoldM f acc ( x ': xs) =
    ConcatMap (FoldM1 f xs) (f @@ acc @@ x )

data FoldM1 :: (TyFun b (TyFun a [b] -> Type) -> Type) -> [a] -> (b ~> [b])
type instance Apply (FoldM1 f xs ) acc = FoldM f acc xs

type family Solutions :: [[Nat]] where
  Solutions = FoldM Place2 '[] (EnumFromTo 1 8)
