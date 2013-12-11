(*===----------------------------------------------------------------------===
 * Main driver code.
 *===----------------------------------------------------------------------===*)

open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts

let interactive () =
  print_endline "The Simple Language 1.0";
  Toplevel.print_prompt ();

  let stream = Lexer.lex false (Stream.of_channel stdin) in
  
  (* Create the JIT. *)
  let the_execution_engine = ExecutionEngine.create Codegen.the_module in
  let the_fpm = PassManager.create_function Codegen.the_module in

  (* Set up the optimizer pipeline.  Start with registering info about how the
  * target lays out data structures. *)
  DataLayout.add (ExecutionEngine.target_data the_execution_engine) the_fpm;
  add_instruction_combination the_fpm;
  add_reassociation the_fpm;
  add_gvn the_fpm;
  add_cfg_simplification the_fpm;
  ignore (PassManager.initialize the_fpm);

  (* Run the main "interpreter loop" *)
  Toplevel.main_loop the_fpm (Some the_execution_engine) stream;
;;


let compile filename =
  print_endline "compiling...";

  let input_channel = open_in filename in

  (* Prime the first token. *)
  let stream = Lexer.lex true (Stream.of_channel input_channel) in
  
  let the_fpm = PassManager.create_function Codegen.the_module in

  (* Run the main "interpreter loop" now. *)
  Toplevel.main_loop the_fpm None stream;

  (* Print out all the generated code to stderr *)
  dump_module Codegen.the_module
;;



let () =
  ignore (initialize_native_target());
  match Sys.argv with
  | [| _; filename |] -> compile filename
  | _                 -> interactive()
