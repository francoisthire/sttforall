open Ast
open Bindlib

let f = fun x -> x

let _ = Format.printf "%b" (f == (fun x -> f x))
