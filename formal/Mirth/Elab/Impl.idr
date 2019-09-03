{-
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Mirth.Elab.Impl
import Mirth.Elab
import Mirth.Syntax
import Mirth.Core
import Mirth.Loc
import Mirth.Test

%access public export
%default total

record MirthEnv where
  constructor MkMirthEnv
  mirthEnvWords : List (String, Loc -> Args -> ElabMon)

record MirthError where
  constructor MkMirthError
  mirthErrorLoc : Loc
  mirthErrorMsg : String

runMirth : Mirth t -> MirthEnv -> Either MirthError (MirthEnv, t)
runMirth (MPure x) env = Right (env, x)
runMirth (MFail loc msg) env = Left (MkMirthError loc msg)
runMirth (MSeek name loc args sig e x f) env =
  case lookup name (mirthEnvWords env) of
    Just defn => do
      (env2, (p, ())) <- assert_total (runMirth (runElabM (defn loc args) sig e x) env)
                                      -- not actually total
      runMirth (f p) env2
    Nothing => Left (MkMirthError loc ("word not defined " ++ name))

runMirth (MWord name preDefn rest) env =
    runMirth rest (MkMirthEnv ((name, defn) :: mirthEnvWords env))
  where
    defn : Loc -> Args -> ElabMon
    defn loc args = MkElabM $ \sig,e,x => do
      p <- preDefn loc args sig e x
      pure (p, ())

-- TODO: Figure out how to avoid dynamic scoping in general.
-- Idea: Use a "scope identity". The env manages all scopes and
-- scope ids, and lookups & elabs happen at specific scope ids.

-- TODO: Figure out how to avoid partiality.
-- Idea: Exploit covariance & contravariance in the environment.

private
implTests : List Test
implTests =
  [ TestCase ( runMirth (MPure ()) (MkMirthEnv [])
             = Right (MkMirthEnv [], ())
             ) {p=Refl}
  , TestCase ( runMirth buildBuiltinEnv (MkMirthEnv [])
             = Right (MkMirthEnv [], ())
             ) {p=Refl}
  ]

