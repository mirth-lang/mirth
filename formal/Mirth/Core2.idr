{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

||| Alternative Core
module Mirth.Core2

%default total
%access public export

infixr 6 <>

data Impl : Type -> Type -> Type where
  PureImpl   : (a -> b) -> Impl a b
  ImpureImpl : (a -> IO b) -> ((b -> Type) -> (a -> Type)) -> Impl a b

cpImpl : Impl a b -> Impl b c -> Impl a c
cpImpl (PureImpl f) (PureImpl g) = PureImpl (g . f)
cpImpl (PureImpl f) (ImpureImpl gio gwp) = ImpureImpl (gio . f) ((. f) . gwp)
cpImpl (ImpureImpl fio fwp) (PureImpl g) = ImpureImpl (map g . fio) (fwp . (. g))
cpImpl (ImpureImpl fio fwp) (ImpureImpl gio gwp) =
  ImpureImpl (\x => fio x >>= gio) (fwp . gwp)

dipImpl : Impl b c -> Impl (a,b) (a,c)
dipImpl (PureImpl f) = PureImpl (\(x,y) => (x,f y))
dipImpl (ImpureImpl fio fwp) =
  ImpureImpl (\(x,y) => map (\z => (x,z)) (fio y))
             (\phi => \(x,y) => fwp (\z => phi (x,z)) y)

mutual
  data Univ : Type where
    WORLD  : Univ
    INT    : Univ
    STR    : Univ
    PACK   : List Univ -> Univ
    WORD   : List Univ -> List Univ -> Univ

  IsPure : Univ -> Type
  IsPure (PACK a) = ArePure a
  IsPure WORLD = Void
  IsPure INT = ()
  IsPure STR = ()
  IsPure (WORD a b) = ()

  ArePure : List Univ -> Type
  ArePure [] = ()
  ArePure (a :: b) = (IsPure a, ArePure b)

  uniqIsPure : (a: Univ) -> (p1,p2: IsPure a) -> p1 = p2
  uniqIsPure WORLD p1 p2 = absurd p1
  uniqIsPure INT () () = Refl
  uniqIsPure STR () () = Refl
  uniqIsPure (PACK xs) p1 p2 = uniqArePure xs p1 p2
  uniqIsPure (WORD xs ys) () () = Refl

  uniqArePure : (a: List Univ) -> (p1,p2: ArePure a) -> p1 = p2
  uniqArePure [] () () = Refl
  uniqArePure (a :: b) (p1a,p1b) (p2a,p2b) with (uniqIsPure a p1a p2a, uniqArePure b p1b p2b)
    uniqArePure (a :: b) (p1a,p1b) (p1a,p1b) | (Refl, Refl) = Refl

  decIsPure : (a: Univ) -> Dec (IsPure a)
  decIsPure (PACK a) = decArePure a
  decIsPure WORLD = No id
  decIsPure INT = Yes ()
  decIsPure STR = Yes ()
  decIsPure (WORD a b) = Yes ()

  decArePure : (a: List Univ) -> Dec (ArePure a)
  decArePure [] = Yes ()
  decArePure (a :: b) with (decIsPure a, decArePure b)
    | (Yes pa, Yes pb) = Yes (pa, pb)
    | (No pa, _) = No (pa . fst)
    | (_, No pb) = No (pb . snd)

  Value : Univ -> Type
  Value  WORLD     = ()
  Value  INT       = Integer
  Value  STR       = String
  Value (PACK a)   = Values a
  Value (WORD a b) = (f: Word ** IsHom f a b)

  Values : List Univ -> Type
  Values [] = ()
  Values (a :: b) = (Value a, Values b)

  valueToStr : (a:Univ) -> Value a -> String
  valueToStr WORLD x = "WORLD"
  valueToStr INT x = show x
  valueToStr STR x = x
  valueToStr (PACK xs) x = valuesToStr xs x
  valueToStr (WORD xs ys) x = "<word>"

  valuesToStr : (a:List Univ) -> Values a -> String
  valuesToStr [] x = ""
  valuesToStr [a] (x,()) = valueToStr a x
  valuesToStr (a::b) (x,y) = valuesToStr b y ++ " " ++ valueToStr a x

  pureToStr : (a: Univ) -> IsPure a -> Value a -> String
  pureToStr a p x = valueToStr a x

  puresToStr : (a: List Univ) -> ArePure a -> Values a -> String
  puresToStr a p x = valuesToStr a x

  data Lit : Type where
    LitInt : Integer -> Lit
    LitStr : String  -> Lit

  LitType : Lit -> Univ
  LitType (LitInt z) = INT
  LitType (LitStr z) = STR

  litValue : (lit: Lit) -> Value (LitType lit)
  litValue (LitInt z) = z
  litValue (LitStr x) = x

  data Word : Type where
    Id   : Word
    (<>) : Word -> Word -> Word
    Push : Lit  -> Word
    Dup  : Word
    Drop : Word
    Swap : Word
    Dip  : Word -> Word
    Put  : Word

  HasDom : (f: Word) -> (a: List Univ) -> Type
  HasDom Id a = ()
  HasDom (x <> y) a = (p: HasDom x a ** HasDom y (inferCod x a p))
  HasDom (Push x) a = ()
  HasDom Dup [] = Void
  HasDom Dup (x :: xs) = IsPure x
  HasDom Drop [] = Void
  HasDom Drop (x :: xs) = IsPure x
  HasDom Swap [] = Void
  HasDom Swap (x :: []) = Void
  HasDom Swap (x :: (y :: xs)) = ()
  HasDom (Dip x) [] = Void
  HasDom (Dip x) (a :: b) = HasDom x b
  HasDom Put [] = Void
  HasDom Put [a] = Void
  HasDom Put (a :: WORLD :: c) = IsPure a
  HasDom Put (a :: INT :: c) = Void
  HasDom Put (a :: STR :: c) = Void
  HasDom Put (a :: PACK _ :: c) = Void
  HasDom Put (a :: WORD _ _ :: c) = Void

  uniqHasDom : (f: Word) -> (a: List Univ) -> (p1,p2: HasDom f a) -> p1 = p2
  uniqHasDom Id a () () = Refl
  uniqHasDom (x <> y) a (p1a ** p1b) (p2a ** p2b) with (uniqHasDom x a p1a p2a)
    uniqHasDom (x <> y) a (p1a ** p1b) (p1a ** p2b) | Refl
      with (uniqHasDom y (inferCod x a p1a) p1b p2b)
        uniqHasDom (x <> y) a (p1a ** p1b) (p1a ** p1b) | Refl | Refl = Refl
  uniqHasDom (Push x) a () () = Refl
  uniqHasDom Dup (a :: b) p1 p2 = uniqIsPure a p1 p2
  uniqHasDom Drop (a :: b)  p1 p2 = uniqIsPure a p1 p2
  uniqHasDom Swap (a::b::c) () () = Refl
  uniqHasDom (Dip x) (a::b) p1 p2 = uniqHasDom x b p1 p2
  uniqHasDom Put (a :: WORLD :: c) p1 p2 = uniqIsPure a p1 p2

  decHasDom : (f: Word) -> (a: List Univ) -> Dec (HasDom f a)
  decHasDom Id a = Yes ()
  decHasDom (x <> y) a with (decHasDom x a)
    decHasDom (x <> y) a | No h = No (\ (p1**p2) => h p1)
    decHasDom (x <> y) a | Yes p1 with (decHasDom y (inferCod x a p1))
      decHasDom (x <> y) a | Yes p1 | Yes p2 = Yes (p1 ** p2)
      decHasDom (x <> y) a | Yes p1 | No h =
         No (\ (p1'**p2) => h (rewrite uniqHasDom x a p1 p1' in p2))
  decHasDom (Push x) a = Yes ()
  decHasDom Dup [] = No id
  decHasDom Dup (x :: xs) = decIsPure x
  decHasDom Drop [] = No id
  decHasDom Drop (x :: xs) = decIsPure x
  decHasDom Swap [] = No id
  decHasDom Swap (x :: []) = No id
  decHasDom Swap (x :: (y :: xs)) = Yes ()
  decHasDom (Dip x) [] = No id
  decHasDom (Dip x) (a :: b) = decHasDom x b
  decHasDom Put [] = No id
  decHasDom Put [a] = No id
  decHasDom Put (a :: WORLD :: c) = decIsPure a
  decHasDom Put (a :: INT :: c) = No id
  decHasDom Put (a :: STR :: c) = No id
  decHasDom Put (a :: PACK _ :: c) = No id
  decHasDom Put (a :: WORD _ _ :: c) = No id

  inferCod : (f: Word) -> (a: List Univ) -> (p: HasDom f a) -> List Univ
  inferCod Id a p = a
  inferCod (x <> y) a (p1 ** p2) = inferCod y (inferCod x a p1) p2
  inferCod (Push x) a p = LitType x :: a
  inferCod Dup (x :: xs) p = x :: x :: xs
  inferCod Drop (x :: xs) p = xs
  inferCod Swap (x :: y :: xs) p = y :: x :: xs
  inferCod (Dip x) (a :: b) p = a :: inferCod x b p
  inferCod Put (a :: WORLD :: c) p = WORLD :: c

  IsHom : (f: Word) -> (a,b: List Univ) -> Type
  IsHom f a b = (p: HasDom f a ** inferCod f a p = b)

  implWord : (f: Word) -> (a: List Univ) -> (p: HasDom f a) ->
             Impl (Values a) (Values (inferCod f a p))
  implWord Id a p = PureImpl id
  implWord (x <> y) a (p1 ** p2) = cpImpl (implWord x a p1) (implWord y (inferCod x a p1) p2)
  implWord (Push x) a p = PureImpl (\y => (litValue x,y))
  implWord Dup (x :: xs) p = PureImpl (\(a,b) => (a,a,b))
  implWord Drop (x :: xs) p = PureImpl snd
  implWord Swap (x :: y :: xs) p = PureImpl (\(a,b,c) => (b,a,c))
  implWord (Dip x) (a :: b) p = dipImpl (implWord x b p)
  implWord Put (a :: WORLD :: c) p =
    ImpureImpl (\ (x,y,z) => putStrLn (pureToStr a p x) >>= const (pure ((),z)))
               (\ phi => phi . snd)

