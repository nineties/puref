(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: vmtypes.mli 2010-07-30 03:54:38 nineties $
 *)

type instruction =
    | ScI of string
    | NumI of int
    | MkappI
    | PushI of int
    | SlideI of int
    | UpdateI of int
    | PopI of int
    | UnwindI

type node =
    | NumN of int (* value *)
    | IndN of node (* expr *)
    | AppN of node * node (* fun, arg *)
    | ScN of string * int * instruction list (* name, # of args, body *)
