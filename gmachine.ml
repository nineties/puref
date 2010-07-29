(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: gmachine.ml 2010-07-30 04:06:28 nineties $
 *)

open Vmtypes

(* table of super combinators *)
let sctable = Hashtbl.create 0

(* VM stack *)
let max_stack_depth = 1024
let stack = Array.create max_stack_depth (NumN 0)
let top = ref (-1)

let push v =
    incr top;
    stack.(!top) <- v

(* for visualize *)
let step = ref 0

let drop n =
    top := !top - n;
    if !top < 0 then raise (Invalid_argument "index out of range")

let getarg = function
    | AppN(f,arg) -> arg
    | _ -> failwith "arg: not reachable"

let rec interpret vis seq =
    vis !step seq stack !top;
    incr step;
    match seq with
    | [] ->
        if !top <> 0 then
            failwith "could not reduce the expression to an integer";
        begin match stack.(!top) with
            | NumN n -> n
            | _ -> failwith "could not reduce the expression to integer"
        end
    | ScI name::is -> begin
        try
            let m = Hashtbl.find sctable name in
            push m;
            interpret vis is
        with Not_found -> failwith ("undefined super combinator: " ^ name)
    end
    | NumI n::is -> push (NumN n); interpret vis is
    | MkappI::is ->
            let f = stack.(!top) in
            let a = stack.(!top-1) in
            drop 2;
            push (AppN(f, a));
            interpret vis is
    | PushI n::is ->
            let app = stack.(!top - n - 1) in
            push (getarg app);
            interpret vis is
    | SlideI n::is ->
            let a = stack.(!top) in
            stack.(!top - n) <- a;
            drop n;
            interpret vis is
    | UpdateI n::is ->
            let a = stack.(!top) in
            stack.(!top - n - 1) <- IndN a;
            drop 1;
            interpret vis is
    | PopI n::is -> drop n; interpret vis is
    | UnwindI::is ->
        begin match stack.(!top) with
            | NumN n        -> n
            | IndN a        -> stack.(!top) <- a; interpret vis seq
            | AppN(f,arg)   -> push f; interpret vis seq
            | ScN(_,_,sc_seq) -> interpret vis sc_seq
        end

let register_sc (name,narg,seq) = Hashtbl.add sctable name (ScN(name,narg,seq))
let register_scs scs = ignore( List.map register_sc scs )

let run vis = interpret vis [ScI "main"; UnwindI]
let run_main () =
    match !Option.visualize with
    | None -> run (fun _ _ _ _ -> ())
    | Some file ->
            let oc = open_out (file ^ ".dot") in
            Format.set_formatter_out_channel oc;
            let ret = run Visualize.drawVM in
            close_out oc;
            Format.set_formatter_out_channel stdout;
            Visualize.compile file;
            ret
