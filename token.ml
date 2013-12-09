(*===----------------------------------------------------------------------===
 * Lexer Tokens
 *===----------------------------------------------------------------------===*)

(* The lexer returns these 'Kwd' if it is an unknown character, otherwise one of
 * these others for known things. *)
type token =
  (* commands *)
  | Def
  | Extern

  (* primary *)
  | Ident of string | Number of int

  (* Control flow *)
  | If | Else

  (* unknown *)
  | Kwd of char

  | BinOp of string

