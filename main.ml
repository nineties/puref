(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: main.ml 2010-07-07 01:50:05 nineties $
 *)

open Format
open Pprint

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.program Lexer.lex lexbuf in
    pp_program std_formatter program;
    exit 0
