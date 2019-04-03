module Mirth.Core3

Rel : Type -> Type -> Type
Rel a b = a -> b -> Type

data ListRel : (rel: Rel a b) -> Rel (List a) (List b) where
  NilRel  : ListRel rel [] []
  ConsRel : rel x y -> ListRel rel xs ys -> ListRel rel (x::xs) (y::ys)

mutual
  data Value : Type where
    TType  : Nat -> Value
    TInt   : Value
    VInt   : Integer -> Value
    TPack  : Stack -> Value
    VPack  : Stack -> Value
    TWord  : Stack -> Stack -> Value
    VWord  : Stack -> Word  -> Value

  data Word : Type where
    WId      : Word
    WCp      : Word -> Word -> Word
    WDip     : Word -> Word
    WSwap    : Word
    WDup     : Word
    WDrop    : Word
    WPush    : Value -> Word
    WPack2   : Word
    WUnpack2 : Word
    WInpack  : Word -> Word

  Stack : Type
  Stack = List Value

mutual

  data SubType : Rel Value Value where
    SubTType : LTE a b -> SubType (TType a) (TType b)
    SubTInt  : SubType TInt TInt
    SubTPack : SubStackType xs ys -> SubType (TPack xs) (TPack ys)
    SubTWord : SubStackType dom2 dom1 ->
               SubStackType cod1 cod2 ->
               SubType (TWord dom1 cod1) (TWord dom2 cod2)

  data SubStackType : Rel Stack Stack where
    SubNil  : SubStackType [] []
    SubCons : SubType x y -> SubStackType xs ys ->
              SubStackType (x::xs) (y::ys)

  TypeLevel : SubType a b -> Nat
  TypeLevel (SubTType {a} {b} _) = b
  TypeLevel SubTInt = 0
  TypeLevel (SubTPack p) = StackTypeLevel p
  TypeLevel (SubTWord p q) = max (StackTypeLevel p) (StackTypeLevel q)

  StackTypeLevel : SubStackType a b -> Nat
  StackTypeLevel SubNil = 0
  StackTypeLevel (SubCons p q) = max (TypeLevel p) (StackTypeLevel q)

  data ValueEq : Rel Value Value where
    EqType  : SubType a b -> SubType b a -> ValueEq a b
    EqVInt  : ValueEq (VInt n) (VInt n)
    EqVPack : StackEq xs ys -> ValueEq (VPack xs) (VPack ys)

  data StackEq : Rel Stack Stack where
    NilEq  : StackEq [] []
    ConsEq : ValueEq x y -> StackEq xs ys -> StackEq (x::xs) (y::ys)

  GetType : ValueEq v1 v2 -> Value
  GetType (EqType p q) = TType (S (TypeLevel p))
  GetType EqVInt = TInt
  GetType (EqVPack p) = TPack (GetStackType p)

  GetStackType : StackEq s1 s2 -> Stack
  GetStackType NilEq = []
  GetStackType (ConsEq p q) = GetType p :: GetStackType q

  data HasDom : (word: Word) -> (dom: Stack) -> Type where
    WIdDom      : HasDom WId dom
    WCpDom      : (p: HasDom w1 dom) -> HasDom w2 (Cod p) -> HasDom (WCp w1 w2) dom
    WDipDom     : HasDom w dom -> HasDom (WDip w) (a::dom)
    WSwapDom    : HasDom WSwap (a::b::dom)
    WDupDom     : HasDom WDup (a::dom)
    WDropDom    : HasDom WDrop (a::dom)
    WPushDom    : ValueEq v v -> HasDom (WPush v) dom
    WPack2Dom   : HasDom WPack2 (a::b::dom)
    WUnpack2Dom : HasDom WUnpack2 (TPack [a,b]::dom)
    WInpackDom  : HasDom w dom' -> HasDom (WInpack w) (TPack dom' :: dom)

  Cod : HasDom w dom -> Stack
  Cod {dom} WIdDom = dom
  Cod (WCpDom p q) = Cod q
  Cod {dom=a::dom} (WDipDom p) = a::Cod p
  Cod {dom=a::b::dom} WSwapDom = b::a::dom
  Cod {dom=a::dom} WDupDom = a::a::dom
  Cod {dom=a::dom} WDropDom = dom
  Cod {dom} (WPushDom p) = GetType p::dom
  Cod {dom=a::b::dom} WPack2Dom = TPack [a,b]::dom
  Cod {dom=TPack [a,b]::dom} WUnpack2Dom = a::b::dom
  Cod {dom=TPack dom' :: dom} (WInpackDom p) = TPack (Cod p) :: dom

  data WordEq : (w1,w2: Word) -> Type where

    -- congruences:

    EqWId   : WordEq WId WId
    EqWCp   : WordEq w1a w2a -> WordEq w1b w2b ->
              WordEq (WCp w1a w1b) (WCp w2a w2b)
    EqWDip  : WordEq w1 w2 -> WordEq (WDip w1) (WDip w2)
    EqWSwap : WordEq WSwap WSwap
    EqWDup  : WordEq WDup WDup
    EqWDrop : WordEq WDrop WDrop
    EqWPush : ValueEq v1 v2 -> WordEq (WPush v1) (WPush v2)
    EqWPack2 : WordEq WPack2 WPack2
    EqWUnpack2 : WordEq WUnpack2 WUnpack2
    EqWInpack : WordEq w1 w2 -> WordEq (WInpack w1) (WInpack w2)

    -- equivalence:

    EqSymm : WordEq p1 p2 -> WordEq p2 p1
    EqTran : WordEq p1 p2 -> WordEq p2 p3 -> WordEq p1 p3

    -- axioms:

    CpCp  : WordEq (WCp w1 (WCp w2 w3)) (WCp (WCp w1 w2) w3)
    CpIdL : WordEq (WCp WId w) w
    CpIdR : WordEq (WCp w WId) w
    DipCp : WordEq (WCp (WDip w1) (WDip w2)) (WDip (WCp w1 w2))
    DipId : WordEq (WDip WId) WId

    SwapSwap : WordEq (WCp WSwap WSwap) WId
    SwapDip2 : WordEq (WCp WSwap (WDip (WDip w))) (W


    InpackCp : WordEq (WCp (WInpack w1) (WInpack w2)) (WInpack (WCp w1 w2))
    InpackId : WordEq (WInpack WId) WId
    Pack2Unpack2: WordEq (WCp WPack2 WUnpack2) WId


