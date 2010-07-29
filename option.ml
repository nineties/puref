(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: option.ml 2010-07-30 04:01:43 nineties $
 *)

let input = ref ""
let dump_parse = ref false
let dump_insn  = ref false
let mark = ref 2
let visualize = ref None

let speclist =
    [("-mark1", Arg.Unit (fun () -> mark := 1), " run as mark1 G-machine");
     ("-mark2", Arg.Unit (fun () -> mark := 2), " run as mark2 G-machine");
     ("-dparse", Arg.Set dump_parse, " dump parsed super combinators");
     ("-dinsn", Arg.Set dump_insn, " dump compiled super combinators");
     ("-vis",   Arg.String (fun file -> visualize := Some file), "NAME visualize execution sequence")
    ]

