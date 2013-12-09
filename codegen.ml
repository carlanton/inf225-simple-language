(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

exception Error of string

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "The Simple Language"
let builder = Llvm.builder context
let named_values:(string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10
let int_type = Llvm.i32_type context
let bool_type = Llvm.i1_type context

let rec codegen_expr = function
  | Ast.Num n -> Llvm.const_int int_type n

  | Ast.Id name ->
    (try Hashtbl.find named_values name with
      | Not_found -> raise (Error "unkown variable name"))

  | Ast.PlusExpr (lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    Llvm.build_add lhs_val rhs_val "addtmp" builder

  | Ast.MulExpr (lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    Llvm.build_mul lhs_val rhs_val "multmp" builder
  
  | Ast.SubtractExpr (lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    Llvm.build_sub lhs_val rhs_val "subtmp" builder

  | Ast.LTExpr (lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    let i = Llvm.build_icmp Llvm.Icmp.Ult lhs_val rhs_val "cmptmp" builder in
    (* cast bool to int (i1 -> i32) *)
    Llvm.build_zext i int_type "booltmp" builder

  | Ast.AndExpr (lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in

    let zero = Llvm.const_int int_type 0 in
    let lhs_bool = Llvm.build_icmp Llvm.Icmp.Ne lhs_val zero "lhsbool" builder in
    let rhs_bool = Llvm.build_icmp Llvm.Icmp.Ne rhs_val zero "rhsbool" builder in

    let and_bool = Llvm.build_and lhs_bool rhs_bool "andtmp" builder in
    Llvm.build_zext and_bool int_type "booltmp" builder


  | Ast.OrExpr (lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in

    let zero = Llvm.const_int int_type 0 in
    let lhs_bool = Llvm.build_icmp Llvm.Icmp.Ne lhs_val zero "lhsbool" builder in
    let rhs_bool = Llvm.build_icmp Llvm.Icmp.Ne rhs_val zero "rhsbool" builder in

    let or_bool = Llvm.build_or lhs_bool rhs_bool "ortmp" builder in
    Llvm.build_zext or_bool int_type "booltmp" builder


  | Ast.EqExpr (lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    let eq_bool = Llvm.build_icmp Llvm.Icmp.Eq lhs_val rhs_val "eqbool" builder in
    Llvm.build_zext eq_bool int_type "booltmp" builder

  | Ast.Call (f, args) ->
    let f = match Llvm.lookup_function f the_module with
      | Some f -> f
      | None -> raise (Error "unkown function referenced")
    in
    let params = Llvm.params f in 

    if Array.length params == Array.length args then () else
      raise (Error "incorrect # arguments passed");

    let args = Array.map codegen_expr args in
    Llvm.build_call f args "calltmp" builder

  | Ast.If (cond, then_, else_) ->
    (* Generate code for cond *)
    let cond = codegen_expr cond in

    (* Convert condition to a bool by comparing equal to 0 *)
    let zero = Llvm.const_int int_type 0 in
    let cond_val = Llvm.build_icmp Llvm.Icmp.Ne cond zero "ifcond" builder in

    (* Grab the first block so that we might later add the conditional
     * branch to it at the end of the function *) 
    let start_bb = Llvm.insertion_block builder in
    let the_function = Llvm.block_parent start_bb in
    
    let then_bb = Llvm.append_block context "then" the_function in

    (* Emit 'then' value *)
    Llvm.position_at_end then_bb builder;
    let then_val = codegen_expr then_ in

    (* Codegen of 'then' can change the current block, update then_bb for
     * the phi. We create a new name because one is used for the phi node,
     * and the other is used for the conditional branch *)
    let new_then_bb = Llvm.insertion_block builder in

    (* Emit 'else' value *)
    let else_bb = Llvm.append_block context "else" the_function in
    Llvm.position_at_end else_bb builder;
    let else_val = codegen_expr else_ in

    (* Codegen of 'else' can change the current block, update else_bb for
     * the phi *)
    let new_else_bb = Llvm.insertion_block builder in

    (* Emit merge block *)
    let merge_bb = Llvm.append_block context "ifcont" the_function in
    Llvm.position_at_end merge_bb builder;
    let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
    let phi = Llvm.build_phi incoming "iftmp" builder in

    (* Return to the start block to add the conditional branch *)
    Llvm.position_at_end start_bb builder;
    ignore (Llvm.build_cond_br cond_val then_bb else_bb builder);

    (* Set an unconditional branch at the end of the 'then' block and the
     * 'else' block to the 'merge' block *)
    Llvm.position_at_end new_then_bb builder;
    ignore (Llvm.build_br merge_bb builder);
    Llvm.position_at_end new_else_bb builder;
    ignore (Llvm.build_br merge_bb builder);

    (* Finally, set the builder to the end of the merge block. *)
    Llvm.position_at_end merge_bb builder;

    phi

let codegen_proto (Ast.Prototype (name, args)) =
  (* Make the function type: int(int,int) etc. *)
  let ints = Array.make (Array.length args) int_type in
  let ft = Llvm.function_type int_type ints in

  let func =
    match Llvm.lookup_function name the_module with
    | None -> Llvm.declare_function name ft the_module

    (* If 'f' conflicted, there was already something named 'name'. If it
     * has a body, don't allow redefinition or reextern. *)
    | Some f ->
      (* If 'f' already has a body, reject this. *)
      if Llvm.block_begin f <> Llvm.At_end f then
        raise (Error "redefinition of function");

      (* If 'f' took a different number of arguments, reject. *)
      if Llvm.element_type (Llvm.type_of f) <> ft then
        raise (Error "redefinition of function with different # args");
      f
  in

  (* Set names for all arguments. *)
  Array.iteri (fun i x ->
    let name = args.(i) in
    Llvm.set_value_name name x;
    Hashtbl.add named_values name x;
  ) (Llvm.params func); (* for each parameter x of func *)

  func

let codegen_func the_fpm (Ast.Function (proto, body)) =
  Hashtbl.clear named_values;
  let the_function = codegen_proto proto in

  (* Create a new basic block to start insertion into. *)
  let bb = Llvm.append_block context "entry" the_function in
  Llvm.position_at_end bb builder;

  try
    let ret_val = codegen_expr body in

    (* Finish off the function. *)
    let _ = Llvm.build_ret ret_val builder in

    (* Validate the generated code, checking for consistency. *)
    Llvm_analysis.assert_valid_function the_function;

    (* Optimize the function. *)
    let _ = Llvm.PassManager.run_function the_function the_fpm in

    the_function
  with e ->
    Llvm.delete_function the_function;
    raise e

