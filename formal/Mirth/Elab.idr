{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Mirth.Mirth

import Mirth.Loc
import Mirth.Syntax
import Mirth.Core

%default total
%access public export

data Mirth : Type -> Type where
  MPure : t -> Mirth t
  MFail : Loc -> String -> Mirth t
  MSeek : String -> Loc -> Args -> (sig: Sig) -> (e: StackEnds) -> (x: Stack sig e)
       -> ((y: Stack sig e ** Word sig x y) -> Mirth t) -> Mirth t
  MWord : String -> (Loc -> Args -> (sig: Sig) -> (e: StackEnds) -> (x: Stack sig e)
                         -> Mirth (y: Stack sig e ** Word sig x y))
       -> Mirth t -> Mirth t

Functor Mirth where
  map f (MPure x) = MPure (f x)
  map f (MFail loc msg) = MFail loc msg
  map f (MSeek name loc args sig end stack m) =
    MSeek name loc args sig end stack (\p => map f (m p))
  map f (MWord name elab m) = MWord name elab (map f m)

Applicative Mirth where
  pure = MPure
  (<*>) (MPure f) m = map f m
  (<*>) (MFail loc msg) m = MFail loc msg
  (<*>) (MSeek name loc args sig end stack m1) m2 =
    MSeek name loc args sig end stack (\p => m1 p <*> m2)
  (<*>) (MWord name elab m1) m = MWord name elab (m1 <*> m)

Monad Mirth where
  (>>=) (MPure x) f = f x
  (>>=) (MFail loc msg) f = MFail loc msg
  (>>=) (MSeek name loc args sig end stack m1) f =
    MSeek name loc args sig end stack (\p => m1 p >>= f)
  (>>=) (MWord name elab m) f = MWord name elab (m >>= f)

fail : Loc -> String -> Mirth t
fail = MFail

RunElabM : Type -> Type
RunElabM t =
  (sig: Sig) -> (e: StackEnds) -> (x: Stack sig e) ->
  Mirth ((y: Stack sig e ** Word sig x y), t)

data ElabM : Type -> Type where
  MkElabM : RunElabM t -> ElabM t

runElabM : ElabM t -> RunElabM t
runElabM (MkElabM m) = m

Functor ElabM where
  map f (MkElabM m) = MkElabM $ \sig,e,x => do
    (p, a) <- m sig e x
    pure (p, f a)

Applicative ElabM where
  pure v = MkElabM $ \sig,e,x =>
    pure ((x ** WId), v)
  (<*>) mf ma = MkElabM $ \sig,e,x => do
    ((y ** w1), f) <- runElabM mf sig e x
    ((z ** w2), a) <- runElabM ma sig e y
    pure ((z ** w1 <> w2), f a)

Monad ElabM where
  (>>=) ma f = MkElabM $ \sig,e,x => do
    ((y ** w1), a) <- runElabM ma sig e x
    ((z ** w2), b) <- runElabM (f a) sig e y
    pure ((z ** w1 <> w2), b)

Semigroup t => Semigroup (ElabM t) where
  (<+>) m1 m2 = do
    a <- m1
    b <- m2
    pure (a <+> b)

Monoid t => Monoid (ElabM t) where
  neutral = pure neutral

ElabMon : Type
ElabMon = ElabM ()

elabFail : Loc -> String -> ElabM t
elabFail loc msg = MkElabM $ \sig,e,x => fail loc msg

lookupWord : String -> Loc -> Args -> ElabMon
lookupWord name loc args = MkElabM $ \sig,e,x =>
  MSeek name loc args sig e x (\p => MPure (p, ()))

defineWord : String -> (Loc -> Args -> ElabMon) -> Mirth ()
defineWord name elab =
  MWord name (\loc,args,sig,e,x =>
    fst <$> runElabM (elab loc args) sig e x) (MPure ())

elabDip : Loc -> ElabM t -> ElabM t
elabDip {t} loc m = MkElabM run
  where
    run : RunElabM t
    run sig e (x' :> x1) = do
      ((y' ** w),v) <- runElabM m sig e x'
      pure ((y' :> x1 ** WDip w),v)
    run sig StackEndsInNil SNil =
      fail loc "dip: Can't dip at empty stack."
    run sig StackEndsInVar (SVar _) =
      fail loc "dip: Can't dip at stack variable."

elabLit  : Loc -> Lit -> ElabMon
elabAtom : Atom -> ElabMon
elabExpr : Expr -> ElabMon
elabLit loc (LInt n) = MkElabM $ \sig,e,x => do
  pure ((x :> DInt ** WInt n), ())
elabLit loc (LStr s) = MkElabM $ \sig,e,x => do
  pure ((x :> DStr ** WStr s), ())
elabAtom (ALit loc lit) = elabLit loc lit
elabAtom (AWord loc name args) = lookupWord name loc args
elabAtom (ALine x y) = pure ()
elabExpr [] = pure ()
elabExpr (atom :: expr) = do
  elabAtom atom
  elabExpr expr

buildBuiltinEnv : Mirth ()
buildBuiltinEnv = do
    simpleDef  "drop"    dropR
    simpleDef  "dup"     dupR
    simpleDef  "swap"    swapR
    defineWord "dip"     dipD
    simpleDef  "pack2"   pack2R
    simpleDef  "unpack2" unpack2R
    defineWord "inpack"  inpackD
  where
    dipD : Loc -> Args -> ElabMon
    dipD loc [expr] = elabDip loc (elabExpr expr)
    dipD loc args = elabFail loc "dip: Expected exactly 1 arg."

    simpleDef : String -> (Loc -> RunElabM ()) -> Mirth ()
    simpleDef name body =
      defineWord name $ \loc,args =>
        case args of
          [] => MkElabM (body loc)
          _  => elabFail loc (name ++ ": Expected exactly 0 args.")

    dupR : Loc -> RunElabM ()
    dupR loc sig e (x :> a) =
      -- TODO: check a is duppable.
      pure ((x :> a :> a ** WDup), ())
    dupR loc sig StackEndsInNil SNil =
      fail loc "dup: Can't dup at empty stack."
    dupR loc sig StackEndsInVar (SVar _) =
      fail loc "dup: Can't dup at stack variable."

    dropR : Loc -> RunElabM ()
    dropR loc sig e (x :> a) =
      -- TODO: check a is droppable.
      pure ((x ** WDrop), ())
    dropR loc sig StackEndsInNil SNil =
      fail loc "drop: Can't drop at empty stack."
    dropR loc sig StackEndsInVar (SVar _) =
      fail loc "drop: Can't drop at stack variable."

    swapR : Loc -> RunElabM ()
    swapR loc sig e (x :> a :> b) =
      pure ((x :> b :> a ** WSwap), ())
    swapR loc sig StackEndsInNil SNil =
      fail loc "swap: Can't swap at empty stack."
    swapR loc sig StackEndsInNil (SNil :> a) =
      fail loc "swap: Can't swap at stack with only 1 element."
    swapR loc sig StackEndsInVar (SVar _) =
      fail loc "swap: Can't swap at stack variable."
    swapR loc sig StackEndsInVar (SVar _ :> a) =
      fail loc "swap: Can't swap at stack with only 1 known element."

    pack2R : Loc -> RunElabM ()
    pack2R loc sig e (x :> a :> b) =
      pure ((x :> DPack (SNil :> a :> b) ** WPack2), ())
    pack2R loc sig StackEndsInNil SNil =
      fail loc "pack2: Can't pack2 at empty stack."
    pack2R loc sig StackEndsInNil (SNil :> a) =
      fail loc "pack2: Can't pack2 at stack with only 1 element."
    pack2R loc sig StackEndsInVar (SVar _) =
      fail loc "pack2: Can't pack2 at stack variable."
    pack2R loc sig StackEndsInVar (SVar _ :> a) =
      fail loc "pack2: Can't pack2 at stack with only 1 known element."

    unpack2R : Loc -> RunElabM ()
    unpack2R loc sig e (x :> DPack (SNil :> a :> b)) =
      pure ((x :> a :> b ** WUnpack2), ())
    unpack2R loc sig e (x :> DPack _) =
      fail loc "unpack2: Can only unpack2 on tuples of size 2."
    unpack2R loc sig e (x :> _) =
      fail loc "unpack2: Can't unpack2 at unknown type (expected tuple of size 2)."
    unpack2R loc sig StackEndsInNil SNil =
      fail loc "unpack2: Can't unpack2 at empty stack."
    unpack2R loc sig StackEndsInVar (SVar _) =
      fail loc "unpack2: Can't unpack2 at stack variable."

    inpackR : Loc -> ElabMon -> RunElabM ()
    inpackR loc body sig e (x :> DPack x') = do
      ((y' ** w), ()) <- runElabM body sig _ x'
      pure ((x :> DPack y' ** WInpack w), ())
    inpackR loc body sig e (x :> _) =
      fail loc "inpack: Can't inpack at unknown type (expected tuple)."
    inpackR loc body sig StackEndsInNil SNil =
      fail loc "inpack: Can't inpack at empty stack."
    inpackR loc body sig StackEndsInVar (SVar _) =
      fail loc "inpack: Can't inpack at stack variable."

    inpackD : Loc -> Args -> ElabMon
    inpackD loc [expr] = MkElabM (inpackR loc (elabExpr expr))
    inpackD loc _ = elabFail loc "inpack: Expected exactly 1 arg."

