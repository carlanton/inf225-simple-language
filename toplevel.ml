(*===----------------------------------------------------------------------===
 * Top-Level parsing and JIT Driver
 *===----------------------------------------------------------------------===*)

open Llvm
open Llvm_executionengine

(* Print the prompt *)
let print_prompt () = 
  print_string "λ: "; flush stdout
;;

(* top ::= definition | external | expression | ';' *)
let rec main_loop the_fpm the_execution_engine stream =
  match Stream.peek stream with
  | None -> ()

  | Some (Token.Kwd '\n' | Token.Kwd ';') ->
      Stream.junk stream;
      main_loop the_fpm the_execution_engine stream

  | Some token ->
      begin
        try match token with
        | Token.Def ->
            let e = Parser.parse_definition stream in
            print_string "A function has beed created: ";
            print_endline (AstUtils.string_of_function e);
            ignore (Codegen.codegen_func the_fpm e);
            

        | Token.Extern ->
            let e = Parser.parse_extern stream in
            print_string "An extern was defined: ";
            print_endline (AstUtils.string_of_proto e);
            ignore (Codegen.codegen_proto e);

        | _ ->
            (* Evaluate a top-level expression into a main function. *)
            let e = Parser.parse_toplevel stream in
            print_string "Top-level expression: ";
            print_endline (AstUtils.string_of_toplevel e);

            let the_function = Codegen.codegen_func the_fpm e in

            match the_execution_engine with
            | Some execution_engine ->
                let result = ExecutionEngine.run_function the_function [||]
                  execution_engine in
                print_string "↳  ";
                print_int (GenericValue.as_int result);
                print_newline ()
            | _ -> ()

        with Stream.Error s | Codegen.Error s ->
          (* Skip token for error recovery. *)
          Stream.junk stream;
          print_endline s;
      end;

  (* Print the prompt if the_execution_engine is set,
   * i.e. interactive mode *)
  match the_execution_engine with
  | Some _ -> print_prompt () ;
  |      _ -> () ;
  ; 

  main_loop the_fpm the_execution_engine stream

