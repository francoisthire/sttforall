open Ast
open Bindlib
open Typing

let x = newvar_ty "A"

let ty =
  let arr = arrow (box_of_var x) (box_of_var x) in
  unbox (forallK x (ty arr))

let te =
  let abs = abs "x" (box_of_var x) (fun var -> box_of_var var) in
  unbox (term (absT x abs))

let _ = Format.printf "%b@." (phas_ptype {ty=[];var=[]} te ty)
