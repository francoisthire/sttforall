open Ast
open Bindlib

type signature

type typing_error

val empty_signature : signature

val phas_ptype : context -> pterm -> pty -> bool

val check_decl : signature -> decl -> (signature, typing_error) result

val check_ast : signature -> ast -> (signature, typing_error) result
