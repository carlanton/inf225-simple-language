(*===----------------------------------------------------------------------===
 * Utils for AST
 *===----------------------------------------------------------------------===*)

open Ast

let rec sepConcat sep sl = match sl with
  | [] -> ""
  | (h::t) ->
      let f curr next = curr ^ sep ^ next in
      List.fold_left f h t

let rec string_of_expr e = match e with
  | Num n -> string_of_int n
  | Id id -> id
  | PlusExpr (e1, e2) -> 
      "PlusExpr(" ^(string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"

  | SubtractExpr (e1, e2) ->
      "SubtractExpr(" ^(string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"

  | MulExpr (e1, e2) ->
      "MulExpr("  ^(string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"

  | LTExpr (e1, e2) ->
      "LTExpr("  ^(string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  
  | AndExpr (e1, e2) ->
      "AndExpr("  ^(string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  
  | OrExpr (e1, e2) ->
      "OrExpr("  ^(string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"

  | EqExpr (e1, e2) ->
      "EqExpr("  ^(string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"

  | Call (f, exprs) ->
      "Call(" ^ f ^ "," ^
        sepConcat "," (Array.to_list (Array.map string_of_expr exprs)) ^ ")"

  | If (x, t, e) ->
      "If(" ^ (string_of_expr x) ^ ","
            ^ (string_of_expr t) ^
        "," ^ (string_of_expr e) ^ ")"

let string_of_proto (Prototype(name, params)) =
  let args = sepConcat "," (Array.to_list params) in
  name ^ "(" ^ args ^ ")"

let string_of_function (Function(p,e)) =
  let head = string_of_proto p in
  let body = string_of_expr e in
  head ^ " = " ^ body

let string_of_toplevel (Function(p,e)) =
  string_of_expr e

