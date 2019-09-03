{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Mirth.Model

Rel : (t:Type) -> Type
Rel t = t -> t -> Type

Reflexive : Rel t -> Type
Reflexive {t} r = (x: t) -> r x x

Symmetric : Rel t -> Type
Symmetric {t} r = (x,y: t) -> (p: r x y) -> r y x

Transitive : Rel t -> Type
Transitive {t} r = (x,y,z: t) -> (p: r x y) -> (q: r y z) -> r x z

record Set where
  constructor MkSet
  Elem    : Type
  Equ     : Rel Elem
  EquRefl : Reflexive Equ
  EquSymm : Symmetric Equ
  EquTran : Transitive Equ

record Fun (a:Set) (b:Set) where
  constructor MkFun
  Apply    : (x: Elem a) -> Elem b
  ApplyEqu : (x1,x2: Elem a) -> (px: Equ a x1 x2) ->
             Equ b (Apply x1) (Apply x2)

FunEqu : (a,b: Set) -> Rel (Fun a b)
FunEqu a b f g =
  (x: Elem a) -> Equ b (Apply f x) (Apply g x)

SetProd0 : Set
SetProd0 = MkSet () (\x,y => ())
  (\x => ()) (\x,y,p => ()) (\x,y,z,p,q => ())

FunProd0 : (a: Set) -> Fun a SetProd0
FunProd0 = MkSetHom (\x => ()) (\x1,x2,px => ())

SetProd2 : Set -> Set -> Set
SetProd2 a b = MkSet E R EquR EquS EquT
  where
    E    : Type
    R    : Rel E
    EquR : Reflexive R
    EquS : Symmetric R
    EquT : Transitive R

    E = (Elem a, Elem b)
    R (x1,y1) (x2,y2) = (Equ a x1 x2, Equ b y1 y2)
    EquR (x,y) = (EquRefl a x, EquRefl b y)
    EquS (x1,y1) (x2,y2) (px,py) = (EquSymm a x1 x2 px, EquSymm b y1 y2 py)
    EquT (x1,y1) (x2,y2) (x3,y3) (px,py) (qx,qy) =
      (EquTran a x1 x2 x3 px qx, EquTran b y1 y2 y3 py qy)

