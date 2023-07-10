open Lexing
open Parser
open Lekser
open Typechecker
open Ast
open Generator
(*
lexer test
let main () =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    ignore (token lexbuf)
  done

let _ = Printexc.print main ()
*)

let top = operations token (Lexing.from_channel stdin)

(*let _ = print_endline "Lexing, parsing passed"*)
let typed = infer top

(*let _ = print_endline "Typechecker passed"*)
let generated = JGen.gen typed

(*let _ = print_endline "Code generation passed"*)
let _ = print_endline generated
