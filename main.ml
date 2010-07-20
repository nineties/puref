(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: main.ml 2010-07-21 00:56:01 nineties $
 *)

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.program Lexer.lex lexbuf in
    Pprint.parsed Format.std_formatter program;
    let scs = Compile.f program in
    Pprint.compiled Format.std_formatter scs;
    Gmachine.register_scs scs;
    Format.printf "result: %d@." (Gmachine.run_main ())
