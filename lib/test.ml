open Core.Std
open Regex

let rec to_string re =
  let open Printf in
  match re with
  | String str -> sprintf "STRING(%s)" str
  | Char c -> sprintf "CHAR(%c)" c
  | Star re -> sprintf "STAR(%s)" (to_string re)
  | Concat (r1, r2) -> sprintf "CONCAT(%s,%s)" (to_string r1) (to_string r2)

let () =
  print_endline @@ to_string @@ Prsr.parse Lxr.read (Lexing.from_channel In_channel.stdin)