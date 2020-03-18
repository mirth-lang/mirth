{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Mirth.Core

%default total
%access public export

infixl 6 :>
infixl 6 +>
infixr 6 <>

record Sig where
  constructor MkSig
  DataVar  : Type
  StackVar : Type

data StackEnds = StackEndsInVar | StackEndsInNil

mutual
  data Data : (sig: Sig) -> Type where
    DVar  : DataVar sig -> Data sig
    DPack : Stack sig e -> Data sig
    DInt  : Data sig
    DStr  : Data sig

  data Stack : (sig: Sig) -> StackEnds -> Type where
    SVar : StackVar sig -> Stack sig StackEndsInVar
    SNil : Stack sig StackEndsInNil
    (:>) : Stack sig p -> Data sig -> Stack sig p

(+>) : (s1: Stack sig e) -> Stack sig StackEndsInNil -> Stack sig e
s1 +> SNil = s1
s1 +> (s2 :> a) = (s1 +> s2) :> a

mutual
  data Word : (sig: Sig) -> (dom, cod: Stack sig e) -> Type where
    WId      : Word sig s s
    (<>)     : Word sig s1 s2 -> Word sig s2 s3 -> Word sig s1 s3
    WSwap    : Word sig (s :> a :> b) (s :> b :> a)
    WDup     : Word sig (s :> a) (s :> a :> a)
    WDrop    : Word sig (s :> a) s
    WDip     : Word sig s1 s2 -> Word sig (s1 :> a) (s2 :> a)
    WPack2   : Word sig (s :> a :> b) (s :> DPack (SNil :> a :> b))
    WUnpack2 : Word sig (s :> DPack (SNil :> a :> b)) (s :> a :> b)
    WInpack  : Word sig s1 s2 -> Word sig (s :> DPack s1) (s :> DPack s2)
    WInt     : Integer -> Word sig s (s :> DInt)
    WStr     : String -> Word sig s (s :> DStr)

weaken : Word sig s1 s2 -> Word sig (s+>s1) (s+>s2)
weaken WId = WId
weaken (x <> y) = weaken x <> weaken y
weaken WSwap = WSwap
weaken WDup = WDup
weaken WDrop = WDrop
weaken (WDip x) = WDip (weaken x)
weaken WPack2 = WPack2
weaken WUnpack2 = WUnpack2
weaken (WInpack x) = WInpack x
weaken (WInt x) = WInt x
weaken (WStr x) = WStr x

dippen : Word sig s1 s2 -> Word sig (s1+>s) (s2+>s)
dippen {s=SNil} w = w
dippen {s=s:>a} w = WDip (dippen w)

mutual
  data Path : (sig: Sig) -> (dom,cod: Stack sig e) -> (lhs,rhs: Word sig dom cod) -> Type where
    PRefl       : Path sig dom cod lhs lhs
    PSymm       : Path sig dom cod lhs rhs -> Path sig dom cod rhs lhs
    PTran       : Path sig dom cod lhs mid -> Path sig dom cod mid rhs ->
                  Path sig dom cod lhs rhs
    PDip        : Path sig dom cod lhs rhs ->
                  Path sig (dom:>a) (cod:>a) (WDip lhs) (WDip rhs)
    PInpack     : Path sig dom cod lhs rhs ->
                  Path sig (s:>DPack dom) (s:>DPack cod) (WInpack lhs) (WInpack rhs)
    PComp       : Path sig dom mid lhs1 rhs1 -> Path sig mid cod lhs2 rhs2 ->
                  Path sig dom cod (lhs1 <> lhs2) (rhs1 <> rhs2)
    PIdL        : Path sig dom cod lhs (WId <> lhs)
    PIdR        : Path sig dom cod lhs (lhs <> WId)
    PAssoc      : Path sig dom cod ((w1 <> w2) <> w3) (w1 <> (w2 <> w3))
    PSwap2      : Path sig (dom:>a:>b) (dom:>a:>b) WId (WSwap <> WSwap)
--  PCommute1   : Path sig (s1 :> a) (s2 :> b)
--                         (WDip f <> weaken {s=s2} {s1=SNil:>a} {s2=SNil:>b} g)
--                         (weaken {s=s1} {s1=SNil:>a} {s2=SNil:>b} g <> WDip f)
    PCommute    : Path sig (s1a +> s1b) (s2a +> s2b)
                           (dippen f <> weaken g)
                           (weaken g <> dippen f)
    PDipId      : Path sig (s:>a) (s:>a) (WDip WId) WId
    PDipCp      : Path sig (s1:>a) (s3:>a) (WDip (w1 <> w2)) (WDip w1 <> WDip w2)
    PInpackId   : Path sig (s:>DPack r1) (s:>DPack r1)
                           (WInpack WId) WId
    PInpackCp   : Path sig (s:>DPack r1) (s:>DPack r3)
                           (WInpack (w1 <> w2)) (WInpack w1 <> WInpack w2)
    PPack2Iso1  : Path sig (s:>a:>b) (s:>a:>b) (WPack2 <> WUnpack2) WId
    PPack2Iso2  : Path sig (s:>DPack (SNil:>a:>b)) (s:>DPack (SNil:>a:>b))
                           (WUnpack2 <> WPack2) WId

-- PCommute : Path sig (s1a +> s1b) (s2a +> s2b) (dippen f <> weaken g) (weaken g <> dippen f)

