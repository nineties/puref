(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: option.mli 2010-07-30 04:01:39 nineties $
 *)

val input : string ref
val dump_parse : bool ref
val dump_insn : bool ref
val mark : int ref
val visualize : (string option) ref

val speclist : (Arg.key * Arg.spec * Arg.doc) list
