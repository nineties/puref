(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: main.ml 2010-07-30 04:00:11 nineties $
 *)

open Option

let usage = "puref file"

let main argv =
    Arg.parse_argv argv (Arg.align speclist) (fun file -> input := file) usage;
    if !input = "" then raise (Arg.Bad "no input file");
    let lexbuf = Lexing.from_channel (open_in !input) in
    let program = Parser.program Lexer.lex lexbuf in
    if !dump_parse then Pprint.parsed Format.std_formatter program; 
    let scs = Compile.f program in
    if !dump_insn then Pprint.compiled Format.std_formatter scs;
    Gmachine.register_scs scs;
    Format.printf "output: %d@." (Gmachine.run_main ())

let _ =
    try
        main Sys.argv
    with
        Arg.Help msg | Arg.Bad msg -> prerr_endline msg; exit 1
