open Ast
open Bindlib
open Typing

let _ = Format.printf "%b" ((fun x -> x) == (fun x -> (fun x -> x) x))
