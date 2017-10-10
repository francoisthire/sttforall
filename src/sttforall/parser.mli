module type STTParser = sig
  type p_ty =
    | P_VarTy of string
    | P_Prop
    | P_Arrow of p_ty * p_ty
    | P_Tyop of string * p_ty list

  type p_pty =
    | P_Ty of p_ty
    | P_ForallK of string * p_pty

  type p_term =
    | P_Var of string
    | P_Abs of string * p_ty * p_term
    | P_App of p_term * p_term
    | P_Impl of p_term * p_term
    | P_Forall of string * p_ty * p_term
    | P_AbsT of string * p_term

  type p_pterm =
    | P_Term of p_term
    | P_ForallT of string * p_pterm

  type p_decl =
    | P_ConstDecl of string * p_pty
    | P_ConstDef of string * p_pty * p_pterm
    | P_TyopDef of string * int
end

module EParser:STTParser

val parse_file : string -> EParser.p_decl list
