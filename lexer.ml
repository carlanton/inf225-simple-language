(*===----------------------------------------------------------------------===
 * Lexer
 *===----------------------------------------------------------------------===*)

let rec lex ignore_newline =
  let rec lex_number buffer = parser
    | [< ' ('0' .. '9' as c); stream >] ->
        Buffer.add_char buffer c;
        lex_number buffer stream
    | [< stream=lex ignore_newline >] ->
        [< 'Token.Number (int_of_string (Buffer.contents buffer)); stream >]
  in

  let rec lex_ident buffer = parser
    | [< ' ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' as c); stream >] ->
        Buffer.add_char buffer c;
        lex_ident buffer stream
    | [< stream=lex ignore_newline >] ->
        match Buffer.contents buffer with
        | "def"    -> [< 'Token.Def; stream >]
        | "extern" -> [< 'Token.Extern; stream >]
        | "if"     -> [< 'Token.If; stream >]
        | "else"   -> [< 'Token.Else; stream >]
        | id -> [< 'Token.Ident id; stream >]
  in 
  
  let rec lex_operator buffer = parser
    | [< ' ('+'|'-'|'*'|'<'|'>'|'='|'&'|'|' as c); stream >] ->
        Buffer.add_char buffer c;
        lex_operator buffer stream
    | [< stream=lex ignore_newline >] ->
        match Buffer.contents buffer with
        | str -> [< 'Token.BinOp str; stream >]
  in 
  
  let rec lex_comment = parser
    | [< ' ('\n'); stream=lex ignore_newline >] -> stream
    | [< 'c; e=lex_comment >] -> e
    | [< >] -> [< >]
  in

  (* The main lexer *)
  parser
    (* Skip any whitespace. *)
    | [< ' (' ' | '\r' | '\t'); stream >] ->
        lex ignore_newline stream

    | [< ' ('\n'); stream >] ->
        if ignore_newline then
          lex ignore_newline stream
        else
          [< 'Token.Kwd '\n'; lex ignore_newline stream >]
      
    (* identifier: [a-zA-Z][a-zA-Z0-9] *)
    | [< ' ('A' .. 'Z' | 'a' .. 'z' as c); stream >] ->
        let buffer = Buffer.create 1 in
        Buffer.add_char buffer c;
        lex_ident buffer stream

    (* number: [0-9]+ *)
    | [< ' ('0' .. '9' as c); stream >] ->
        let buffer = Buffer.create 1 in
        Buffer.add_char buffer c;
        lex_number buffer stream

    (* Comment until end of line. *)
    | [< ' ('#' as c); stream >] ->
        let buffer = Buffer.create 1 in
        Buffer.add_char buffer c;
        lex_operator buffer stream

    | [< ' ('+'|'-'|'*'|'<'|'>'|'='|'&'|'|' as c); stream >] ->
        let buffer = Buffer.create 1 in
        Buffer.add_char buffer c;
        lex_operator buffer stream

    (* Otherwise, just return the character as its ascii value. *)
    | [< 'c; stream >] ->
        [< 'Token.Kwd c; lex ignore_newline stream >]

    (* end of stream. *)
    | [< >] -> [< >]

