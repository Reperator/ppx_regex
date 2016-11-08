type t =
  | String of string
  | Char of char
  | Star of t
  | Concat of t * t
