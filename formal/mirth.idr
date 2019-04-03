{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

-- ugly draft formalization of Mirth's simple "forward elaboration"

module Mirth

import Data.List

Name : Type
Name = String

record TypeSig where
  constructor MkTypeSig
  ValueTypeVarNames : List Name
  StackTypeVarNames : List Name

ValueTypeVar : TypeSig -> Type
ValueTypeVar sig = (x: Name ** Elem x (ValueTypeVarNames sig))

StackTypeVar : TypeSig -> Type
StackTypeVar sig = (x: Name ** Elem x (StackTypeVarNames sig))

data ValueType : (sig: TypeSig) -> Type where
  VTVar : ValueTypeVar sig -> ValueType sig
  VTNat : ValueType sig

infixl 6 <>
data StackType : (sig: TypeSig) -> Type where
  STVar : StackTypeVar sig -> StackType sig
  STNil : StackType sig
  (<>)  : StackType sig -> ValueType sig -> StackType sig


data Word : {sig: TypeSig} -> StackType sig -> StackType sig -> Type where
  WId   : Word xs xs
  WCp   : Word xs ys -> Word ys zs -> Word xs zs
  WSwap : Word (xs <> x <> y) (xs <> y <> x)
  WDrop : Word (xs <> x) xs
  WDup  : Word (xs <> x) (xs <> x <> x)
  WDip  : Word xs ys -> Word (xs <> x) (ys <> x)
  WPlus : Word (xs <> VTNat <> VTNat) (xs <> VTNat)

data Expr : Type where
  EComp : List Expr -> Expr
  ESwap : Expr
  EDrop : Expr
  EDup  : Expr
  EDip  : Expr -> Expr
  EPlus : Expr


data ElabError : (sig: TypeSig) -> Type where
  BadStackType : (expected: List (Maybe (ValueType sig))) ->
                 (got: StackType sig) ->
                 ElabError sig

RunMirth : (sig: TypeSig) -> Type -> Type
RunMirth sig t = (xs: StackType sig) ->
                 Either (ElabError sig)
                        ((ys: StackType sig ** Word xs ys), t)


record Mirth (sig: TypeSig) (t: Type) where
  constructor MkMirth
  runMirth : RunMirth sig t

Functor (Mirth sig) where
  map f (MkMirth p) = MkMirth (\xs => map (map f) (p xs))

Applicative (Mirth sig) where
  pure x = MkMirth (\xs => Right ((xs ** WId), x))
  (<*>) (MkMirth pf) (MkMirth px) =
    MkMirth $ \xs0 => do
      ((xs1 ** w1), f) <- pf xs0
      ((xs2 ** w2), x) <- px xs1
      pure ((xs2 ** WCp w1 w2), f x)

Monad (Mirth sig) where
  (>>=) (MkMirth px) f =
    MkMirth $ \xs0 => do
      ((xs1 ** w1), x) <- px xs0
      ((xs2 ** w2), r) <- runMirth (f x) xs1
      pure ((xs2 ** WCp w1 w2), r)

dip : Mirth sig t -> Mirth sig t
dip (MkMirth p) = MkMirth d
  where
    d : RunMirth sig t
    d (xs <> x) = do
      ((ys ** w), v) <- p xs
      Right ((ys <> x ** WDip w), v)
    d st = Left (BadStackType [Nothing] st)



elab : Expr -> Mirth sig ()
elab (EComp es) = for_ es elab
elab (EDip e) = dip (elab e)

elab ESwap = MkMirth s
  where
    s : RunMirth sig ()
    s (xs <> x <> y) = Right ((xs <> y <> x ** WSwap), ())
    s st = Left (BadStackType [Nothing, Nothing] st)

elab EDrop = MkMirth s
  where
    s : RunMirth sig ()
    s (xs <> x) = Right ((xs ** WDrop), ())
    s st = Left (BadStackType [Nothing] st)

elab EDup = MkMirth s
  where
    s : RunMirth sig ()
    s (xs <> x) = Right ((xs <> x <> x ** WDup), ())
    s st = Left (BadStackType [Nothing] st)

elab EPlus = MkMirth s
  where
    s : RunMirth sig ()
    s (xs <> VTNat <> VTNat) = Right ((xs <> VTNat ** WPlus), ())
    s st = Left (BadStackType [Just VTNat, Just VTNat] st)

