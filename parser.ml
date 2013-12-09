(*===---------------------------------------------------------------------===
 * Parser
 *===---------------------------------------------------------------------===*)

open Printf

(* binop_precedence - This holds the precedence for each binary operator that is
 * defined *)
let binop_precedence:(string, int) Hashtbl.t = Hashtbl.create 10

let () =
  Hashtbl.add binop_precedence "==" 4;
  Hashtbl.add binop_precedence "&&" 5;
  Hashtbl.add binop_precedence "||" 5;
  Hashtbl.add binop_precedence "<" 10;
  Hashtbl.add binop_precedence ">" 10;
  Hashtbl.add binop_precedence "+" 20;
  Hashtbl.add binop_precedence "-" 20;
  Hashtbl.add binop_precedence "*" 40;     (* highest. *)
;;

(* precedence - Get the precedence of the pending binary operator token. *)
let precedence c = try Hashtbl.find binop_precedence c with Not_found -> -1

(* primary
 *   ::= identifier
 *   ::= numberexpr
 *   ::= parenexpr *)
let rec parse_primary = parser
  (* numberexpr ::= number *)
  | [< 'Token.Number n >] -> Ast.Num n

  | [< 'Token.BinOp "-"; 'Token.Number n >] -> Ast.Num (-n)

  (* parenexpr ::= '(' expression ')' *)
  | [< 'Token.Kwd '('; e=parse_expr; 'Token.Kwd ')' ?? "expected ')'" >] -> e

  (* identifierexpr
   *   ::= identifier
   *   ::= identifier '(' argumentexpr ')' *)
  | [< 'Token.Ident id; stream >] ->
      let rec parse_args accumulator =
          match Stream.peek stream with
          (* Hack for handling  function calls with zero arguments.
           * There must be a better way :-) TODO *)
          | Some (Token.Kwd ')') -> begin parser [< >] -> accumulator end 
          | _ -> parser
             | [< e=parse_expr; stream >] ->
                begin parser
                  | [< 'Token.Kwd ','; e=parse_args (e :: accumulator) >] -> e
                  | [< >] -> e :: accumulator
                end stream
      in

      let rec parse_ident id = parser
        (* Function call *)
        | [< 'Token.Kwd '(';
             args=parse_args [];
             'Token.Kwd ')' ?? "expected ')'">] ->
            Ast.Call (id, Array.of_list (List.rev args))

        (* Simple variable ref. *)
        | [< >] -> Ast.Id id
      in

      parse_ident id stream

  (* If Then Else *)
  | [< 'Token.If; c=parse_expr;
       'Token.Kwd ':' ?? "expected ':'"; t=parse_expr;
       'Token.Else ?? "expected 'else'"; 'Token.Kwd ':'; e=parse_expr >] ->
           Ast.If (c, t, e)

  | [< >] -> raise (Stream.Error "unknown token when expecting an expression.")

(* binoprhs
 *   ::= ('+' primary)* *)
and parse_bin_rhs expr_prec lhs stream =
  match Stream.peek stream with
  (* If this is a binop, find its precedence. *)
  | Some (Token.BinOp str) when Hashtbl.mem binop_precedence str ->
      let token_prec = precedence str in

      (* If this is a binop that binds at least as tightly as the current binop,
       * consume it, otherwise we are done. *)
      if token_prec < expr_prec then lhs else begin
        (* Eat the binop. *)
        Stream.junk stream;

        (* Parse the primary expression after the binary operator. *)
        let rhs = parse_primary stream in

        (* Okay, we know this is a binop. *)
        let rhs =
          match Stream.peek stream with
          | Some (Token.BinOp str2) ->
              (* If BinOp binds less tightly with rhs than the operator after
               * rhs, let the pending operator take rhs as its lhs. *)
              let next_prec = precedence str2 in
              if token_prec < next_prec
              then parse_bin_rhs (token_prec + 1) rhs stream
              else rhs
          | _ -> rhs
        in

        (* Merge lhs/rhs. *)
        let lhs = match str with
         | "+" -> Ast.PlusExpr(lhs, rhs)
         | "*" -> Ast.MulExpr(lhs, rhs)
         | "-" -> Ast.SubtractExpr(lhs, rhs)
         | "<" -> Ast.LTExpr(lhs, rhs)
         | ">" -> Ast.LTExpr(rhs, lhs)
         | "&&" -> Ast.AndExpr(lhs, rhs)
         | "||" -> Ast.OrExpr(lhs, rhs)
         | "==" -> Ast.EqExpr(lhs, rhs)
         | _ -> raise (Stream.Error "undefined operator")
        in 
        parse_bin_rhs expr_prec lhs stream
      end
  | _ -> lhs

(* expression
 *   ::= primary binoprhs *)
and parse_expr = parser
  | [< lhs=parse_primary; stream >] -> parse_bin_rhs 0 lhs stream

(* prototype
 *   ::= id '(' id* ')' *)
let parse_prototype =
  let rec parse_args accumulator = parser
    | [< 'Token.Ident id ; stream >] -> parse_args (id :: accumulator) stream
    | [< >] -> accumulator
  in
  parser
  | [< 'Token.Ident id; args=parse_args []; >]  ->
      Ast.Prototype (id, Array.of_list (List.rev args))

  | [< >] ->
      raise (Stream.Error "expected function name in prototype")

(* definition ::= 'def' prototype expression *)
let parse_definition = parser
  | [< 'Token.Def; p=parse_prototype; 'Token.BinOp "="; e=parse_expr >] ->
      Ast.Function (p, e)

(* toplevelexpr ::= expression *)
let parse_toplevel = parser
  | [< e=parse_expr >] ->
      (* Make an anonymous proto. *)
      Ast.Function (Ast.Prototype ("", [||]), e)

(*  external ::= 'extern' prototype *)
let parse_extern = parser
  | [< 'Token.Extern; e=parse_prototype >] -> e
