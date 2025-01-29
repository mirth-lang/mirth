" This Source Code Form is subject to the terms of the Mozilla Public
" License, v. 2.0. If a copy of the MPL was not distributed with this
" file, You can obtain one at https://mozilla.org/MPL/2.0/.

if exists("b:current_syntax")
  finish
endif

" all the characters that can be in a word
" i.e. the ascii letters, digits, and symbols
" excluding ,()[]{}\`.:
syntax iskeyword 33,36-39,42-43,45,47-57,59-90,94-95,97-122,124,126

" reserved words -- these have special syntactic meaning
syntax keyword mirthReserved module import inline alias data struct def def-type def-missing external table field embed-str buffer max-mirth-revision min-mirth-revision patch --
syntax keyword mirthSpecial -> \\

" words, numbers, and types
" atrocious logic to try and lex numbers as numbers and not as words.
syntax match mirthWord "\v[+-]"
syntax match mirthNumber "\v[+-]?[0-9](_[0-9])*(u8|u16|u32|u64|i8|i16|i32|i64)?"
syntax match mirthNumber "\v[+-]?0o[0-7](_[0-7])*(u8|u16|u32|u64|i8|i16|i32|i64)?"
syntax match mirthNumber "\v[+-]?0x[0-9a-fA-F](_?[0-9a-fA-F])*(u8|u16|u32|u64|i8|i16|i32|i64)?"
syntax match mirthWord "\v[a-z_?*~!\@\$%\^&=/\|<>';][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>';]*"
syntax match mirthWord "\v\+[a-z_\-+?*~!\@\$%\^&=/\|<>';][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>';]*"
syntax match mirthWord "\v-[A-Za-z_\-+?*~!\@\$%\^&=/\|<>';][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>';]*"
syntax match mirthType "\v[A-Z][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>';]*"
syntax match mirthTag "\v[\?!\+][A-Z][0-9a-zA-Z_\-?+*~!\@\$%\^&=/\|<>';]*"

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
