" This Source Code Form is subject to the terms of the Mozilla Public
" License, v. 2.0. If a copy of the MPL was not distributed with this
" file, You can obtain one at https://mozilla.org/MPL/2.0/.

if exists("b:current_syntax")
  finish
endif

" all the characters that can be in a word
" i.e. the ascii letters, digits, and symbols
" excluding ,()[]{}\`
" but also allowing : for the sake of reserving that syntax
syntax iskeyword 33,36-39,42-43,45-57,59-90,94-95,97-122,124,126

" reserved words -- these have special syntactic meaning
syntax keyword mirthReserved export import type data end = --
syntax keyword mirthSpecial -> \\

" prelude words
syntax keyword mirthWord dup drop id swap dip nip tuck over rotl rotr par both
syntax keyword mirthWord dup2 drop2 swap2 dip2 nip2 tuck2 over2
syntax keyword mirthWord dup3 drop3
syntax keyword mirthWord true false if and or not xor while iff? and? or? xor? implies? iff?
syntax keyword mirthWord inpack pack2 unpack2 pack0 unpack0 pack1 unpack1 pack3 unpack3 pack4 unpack4

" words, numbers, and types
" atrocious logic to try and lex numbers as numbers and not as words.
syntax match mirthWord "\v[+-]"
syntax match mirthNumber "\v[+-][0-9][0-9]*"
syntax match mirthNumber "\v[+-]0[xX][0-9a-fA-F][0-9a-fA-F]*"
syntax match mirthNumber "\v[0-9][0-9]*"
syntax match mirthNumber "\v0[xX][0-9a-fA-F][0-9a-fA-F]*"
syntax match mirthWord "\v[a-z_?*~!\@\$%\^&=/\|<>'][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"
syntax match mirthWord "\v\+[a-z_\-+?*~!\@\$%\^&=/\|<>'][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"
syntax match mirthWord "\v-[A-Za-z_\-+?*~!\@\$%\^&=/\|<>'][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"
syntax match mirthWord "\v[+-][1-9][0-9]*[A-Za-z_\-+?*~!\@\$%\^&=/\|<>'][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"
syntax match mirthWord "\v[+-]0[A-WYZa-wyz_\-?+*~!\@\$%\^&=/\|<>'][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"
syntax match mirthWord "\v[+-]0[0-9][0-9]*[A-Za-z_\-?+*~!\@\$%\^&=/\|<>'][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"
syntax match mirthWord "\v[+-]0[xX][0-9a-fA-F]*[G-Zg-z_\-?+*~!\@\$%\^&=/\|<>'][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"
syntax match mirthWord "\v[1-9][0-9]*[A-Za-z_\-?+*~!\@\$%\^&=/\|<>'][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"
syntax match mirthWord "\v0[A-WYZa-wyz_\-?+*~!\@\$%\^&=/\|<>'][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"
syntax match mirthWord "\v0[0-9][0-9]*[A-Za-z_\-?+*~!\@\$%\^&=/\|<>'][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"
syntax match mirthWord "\v0[xX][0-9a-fA-F]*[G-Zg-z_\-?+*~!\@\$%\^&=/\|<>'][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"
syntax match mirthType "\v[A-Z][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"
syntax match mirthTag "\v\+[A-Z][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>']*"

" operators
syntax match mirthColon "\v:"
syntax match mirthParen "\v[\(\)]"
syntax match mirthComma "\v,"

" comments
syntax match mirthComment "\v#.*$"
syntax match mirthDocument "\v^[ \t]*\|\|\|.*$"

" strings
syntax region mirthString start=/\v"/ skip=/\v\\./ end=/\v"/

highlight def link mirthReserved Keyword
highlight def link mirthSpecial Keyword
highlight def link mirthColon Operator
highlight def link mirthParen Operator
highlight def link mirthComma Operator
highlight def link mirthWord Function
highlight def link mirthType Type
highlight def link mirthTag Tag
highlight def link mirthComment Comment
highlight def link mirthDocument Comment
highlight def link mirthString String
highlight def link mirthNumber Number

let b:current_syntax = "mirth"
