(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: visualize.ml 2010-07-30 04:04:10 nineties $
 *)

(* generate Graphviz source *)

open Format
open Vmtypes

let node_id = ref (-1)
let new_id () = incr node_id; !node_id

let put_insn = function
    | ScI name  -> printf "sc \\\"%s\\\"" name
    | NumI n    -> printf "num %d" n
    | MkappI    -> printf "mkapp"
    | PushI n   -> printf "push %d" n
    | SlideI n  -> printf "slide %d" n
    | UpdateI n -> printf "update %d" n
    | PopI  n   -> printf "pop %d" n
    | UnwindI   -> printf "unwind"

let max_insn_len = 4
let rec put_insnseq n = function
    | []    -> ()
    | _ when n >= max_insn_len -> printf "..."
    | [i]   -> if n = 0 then printf "<i0> "; put_insn i
    | i::is ->
            if n = 0 then printf "<i0> ";
            put_insn i;
            printf "|";
            put_insnseq (n+1) is

let draw_insnseq seq =
    printf "\tinsn [shape = record,\n";
    printf "\t      label = \"";
    put_insnseq 0 seq;
    printf "\"];\n"

let nodes = ref []
let get_id nd = 
    let rec loop = function
        | []    -> begin
            let id = new_id () in
            nodes := (nd,id) :: !nodes;
            (id, true)
        end
        | (n,id)::_ when n == nd -> (id, false)
        | _::rs -> loop rs
    in loop !nodes

let rec draw_edge_from_node i node color =
    let (id,c) = get_id node in
    let p s d = printf "\tn%d -> n%d [color=\"%s\"];\n" s d color in
    match node with 
    | NumN n    -> p i id
    | IndN nd   -> p i id; if c then draw_edge_from_node id nd "black"
    | AppN(f,a)  ->
        p i id;
        if c then begin
        draw_edge_from_node id f "red";
        draw_edge_from_node id a "blue";
        end
    | ScN(_,_,_)   -> p i id

let draw_edge_from_stack i node =
    let (id,c) = get_id node in
    let p s d = printf "\tstack:s%d -> n%d;\n" s d in
    match node with
    | NumN n    -> p i id
    | IndN nd   -> p i id; if c then draw_edge_from_node id nd "black"
    | AppN(f,a)  ->
        p i id;
        if c then begin
            draw_edge_from_node id f "red";
            draw_edge_from_node id a "blue";
        end
    | ScN(_,_,_)   -> p i id

let draw_edges stack top =
    for i = 0 to top do
        draw_edge_from_stack i stack.(i)
    done

let draw_node (node,id) = 
    match node with
    | NumN n        -> printf "\tn%d [label=\"%d\"];\n" id n
    | IndN _        -> printf "\tn%d [label=\"*\"];\n" id
    | AppN(_,_)     -> printf "\tn%d [label=\"app\"];\n" id
    | ScN(name,_,_) -> printf "\tn%d [label=\"\\\"%s\\\"\"];\n" id name

let draw_nodes () = List.iter (fun t -> draw_node t) !nodes

let draw_stack stack top =
    printf "\tstack [shape = record,\n";
    printf "\t       label = \"";
    for i = top downto 0 do
        printf "<s%d> %d" i (top-i);
        if i > 0 then printf "|"
    done;
    printf "\"];\n";
    draw_edges stack top;
    draw_nodes ()

let drawVM i seq stack top = 
    nodes := [];
    printf "digraph step%d {\n" i;
    printf "\torientation=landscape;\n";
    printf "\tsize=\"10,8\";\n";
    printf "\trankdir=LR;\n";
    printf "\tlabel=\"step %d\";\n" i;
    printf "\tlabelloc=t;\n";
    draw_insnseq seq;
    printf "\tnext [label=\"next\", shape=plaintext];\n";
    printf "\tnext -> insn:i0;\n";
    if top >= 0 then begin
        draw_stack stack top;
        printf "\ttop [label=\"top\", shape=plaintext];\n";
        printf "\ttop -> stack:s%d;\n" top;
        printf "\tinsn:i0 -> top [color=\"white\"];\n";
    end;
    printf "}\n"

let compile file =
    ignore( Sys.command (sprintf "dot -Tps %s.dot -o %s.ps" file file) );
    ignore( Sys.command (sprintf "ps2pdf %s.ps" file) )

