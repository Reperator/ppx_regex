# ppx_regex
OCaml syntax extension for matching with regular expressions

`ppx_regex` adds support for regular expresssions in pattern matchings.
Currently it is in a very early state and by no means ready to be used.
At the moment it uses `ocaml-re` to evaluate the regular expressions although I am working on generating custom DFAs.

Usage:
```ocaml
# #require "ppx_regex";;
# match%regex "abcccc" with "abc*" -> true | _ -> false;;
- : bool = true
```
