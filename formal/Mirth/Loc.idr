||| Location type.
module Mirth.Loc

%access public export
%default total

record Loc where
  constructor MkLoc
  locPath : Maybe String
  locRow  : Integer
  locCol  : Integer

TABSIZE : Integer
TABSIZE = 8

initialLoc : (path: Maybe String) -> Loc
initialLoc p = MkLoc p 1 1

advanceCol : Loc -> Loc
advanceCol (MkLoc p r c) = MkLoc p r (c + 1)

advanceTab : Loc -> Loc
advanceTab (MkLoc p r c) = assert_total (MkLoc p r (c + TABSIZE - mod c TABSIZE))

advanceRow : Loc -> Loc
advanceRow (MkLoc p r c) = MkLoc p (r + 1) 1

advanceChar : Char -> Loc -> Loc
advanceChar '\t' = advanceTab
advanceChar '\n' = advanceRow
advanceChar  _   = advanceCol

advanceChars : List Char -> Loc -> Loc
advanceChars cs loc = foldl (flip advanceChar) loc cs

advanceStr : String -> Loc -> Loc
advanceStr = advanceChars . unpack

locPretty : Loc -> String
locPretty (MkLoc Nothing  r c) = show r ++ ":" ++ show c
locPretty (MkLoc (Just p) r c) = p ++ ":" ++ show r ++ ":" ++ show c

locPrefix : Loc -> String
locPrefix loc = locPretty loc ++ ": "

