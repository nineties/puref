(*
 * puref - 
 * Copyright (C) 2010 nineties
 * 
 * $Id: lexer.mll 2010-06-28 00:15:54 nineties $
 *)

{
open Parser
}

let space  = [' ' '\t' '\n' '\r']
let letter = ['A'-'Z' 'a'-'z' '_']
let digit  = ['0'-'9']

rule lex = parse
    space +
    { lex lexbuf }
    | '#' [^ '\n']* (* comment *)
    { lex lexbuf }
    | letter (letter | digit)*
    { match Lexing.lexeme lexbuf with
          "let" -> Tlet
        | "letrec" -> Tletrec
        | "in" -> Tin
        | "case" -> Tcase
        | "of" -> Tof
        | "Pack" -> Tpack
        | s -> Tvar s }
    | (['0'-'9']* as num) { Tint (int_of_string num) }
    | '{' { Tlbrace }
    | '}' { Trbrace }
    | '(' { Tlparen }
    | ')' { Trparen }
    | '\\' { Tbackslash }
    | ';' { Tsemi }
    | '=' { Tequal }
    | '+' { Tplus }
    | '-' { Tminus }
    | '*' { Tmul }
    | '/' { Tdiv }
    | '<' { Tless }
    | "<=" { Tlessequal }
    | "==" { Tequalequal }
    | "~=" { Tnotequal }
    | ">=" { Tgreaterequal }
    | '>' { Tgreater }
    | '&' { Tand }
    | '|' { Tor }
    | "->" { Tarrow }
    | ',' { Tcomma }
    | '.' { Tdot }
    | eof { Teof }

