(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: gmachine.ml 2010-07-29 20:03:35 nineties $
 *)

type instruction =
    | ScI of string
    | NumI of int
    | MkappI
    | PushI of int
    | UpdateI of int
    | PopI of int
    | UnwindI

type node =
    | NumN of int
    | IndN of node
    | AppN of node * node
    | ScN of int * instruction list

(* table of super combinators *)
let sctable = Hashtbl.create 0

(* VM stack *)
let max_stack_depth = 1024
let stack = Array.create max_stack_depth (NumN 0)
let top = ref (-1)

let push v =
    incr top;
    stack.(!top) <- v

let drop n =
    top := !top - n;
    if !top < 0 then raise (Invalid_argument "index out of range")

let getarg = function
    | AppN(f,arg) -> arg
    | _ -> failwith "arg: not reachable"

let rec interpret seq =
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
            interpret is
        with Not_found -> failwith ("undefined super combinator: " ^ name)
    end
    | NumI n::is -> push (NumN n); interpret is
    | MkappI::is ->
            let f = stack.(!top) in
            let a = stack.(!top-1) in
            drop 2;
            push (AppN(f, a));
            interpret is
    | PushI n::is ->
            let app = stack.(!top - n - 1) in
            push (getarg app);
            interpret is
    | UpdateI n::is ->
            let a = stack.(!top) in
            stack.(!top - n - 1) <- IndN a;
            drop 1;
            interpret is
    | PopI n::is -> drop n; interpret is
    | UnwindI::is ->
        begin match stack.(!top) with
            | NumN n -> n
            | IndN a -> stack.(!top) <- a; interpret seq
            | AppN(f,arg) -> push f; interpret seq
            | ScN(n,sc_seq) -> interpret sc_seq 
        end

let register_sc (name,narg,seq) = Hashtbl.add sctable name (ScN(narg,seq))
let register_scs scs = ignore( List.map register_sc scs )
let run_main () = interpret [ScI "main"; UnwindI]
