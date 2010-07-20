(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: gmachine.ml 2010-07-21 00:54:41 nineties $
 *)

open Syntax

type instruction =
    | ScI of string
    | NumI of int
    | MkappI
    | PushI of int
    | SlideI of int
    | UnwindI

type node =
    | NumN of int
    | AppN of node * node
    | ScN of int * instruction list

let sctable = Hashtbl.create 0

let arg = function
    | AppN(f,arg) -> arg
    | _ -> failwith "arg: not reachable"

let rec drop ls n = if n == 0 then ls else drop (List.tl ls) (n - 1)

let rec interpret seq stack =
    match (seq, stack) with
    | [], NumN n::_ -> n
    | ScI name::is, _ -> begin
        try
            let m = Hashtbl.find sctable name in
            interpret is (m::stack)
        with Not_found -> failwith ("undefined super combinator: " ^ name)
    end
    | NumI n::is, _ -> interpret is (NumN n::stack)
    | MkappI::is, a0::a1::ss -> interpret is (AppN(a0, a1)::ss)
    | PushI n::is, _ ->
            let app = List.nth stack (n + 1) in
            interpret is (arg app::stack)
    | SlideI n::is, a0::ss  -> interpret is (a0::drop ss n)
    | UnwindI::_, NumN n::_ -> n
    | UnwindI::_, AppN(f,arg)::_ -> interpret seq (f::stack)
    | UnwindI::is, ScN(n,sc_seq)::s -> interpret sc_seq stack
    | _ -> failwith "interpret: not reachable"

let register_sc (name,narg,seq) = Hashtbl.add sctable name (ScN(narg,seq))
let register_scs scs = ignore( List.map register_sc scs )
let run_main () = interpret [ScI "main"; UnwindI] []
