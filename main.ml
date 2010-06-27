(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: main.ml 2010-06-28 00:21:17 nineties $
 *)

let _ =
    let lexbuf = Lexing.from_channel stdin in
    Parser.program Lexer.lex lexbuf in
    exit 0
