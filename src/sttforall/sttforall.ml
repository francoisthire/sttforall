open Ast
open Bindlib
open Typing

let x = newvar_ty "A"

let ty =
  let arr = arrow (box_of_var x) (box_of_var x) in
  unbox (forallK x (ty arr))

let _ =
  let lines = Parser.parse_file Sys.argv.(1) in
  let lines' = List.map (Scoping.to_ast) lines in
  let sg = Typing.empty_signature in
  match check_ast sg lines' with
  | Ok sg -> Format.printf "success@."
  | Error e -> Format.printf "error@."
