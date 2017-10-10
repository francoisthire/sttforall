open Ast
open Parser

module EP = EParser

val to_pty : EP.p_pty -> pty

val to_pte : EP.p_pterm -> pterm

val to_ast : EP.p_decl -> decl
