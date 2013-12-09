(*===----------------------------------------------------------------------===
 * Abstract Syntax Tree
 *===----------------------------------------------------------------------===*)

(* Expressions *)
type expr =
  | Id of string                (* identifier *)
  | Num of int                  (* number *)
  | Call of string * expr array (* function calls *)
  | PlusExpr of expr * expr     (* expr1 + expr2 *)
  | MulExpr of expr * expr      (* expr1 * expr2 *)
  | SubtractExpr of expr * expr (* expr1 - expr2 *)
  | LTExpr of expr * expr       (* expr1 < expr2 *)
  | AndExpr of expr * expr      (* expr1 && expr2 *)
  | OrExpr of expr * expr       (* expr1 || expr2 *)
  | EqExpr of expr * expr       (* expr1 == expr2 *)
  | If of expr * expr * expr    (* If expr1 Then expr2 Else expr3 *)

(* Function prototype *)
(* name (args[]) *)
type proto = Prototype of string * string array

(* Function *)
(* name (args[]) = expr *)
type func = Function of proto * expr

