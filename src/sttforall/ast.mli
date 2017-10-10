open Bindlib

(* TODO
   - add the constant case
*)

type tyop

type ty =
  | TyVar of ty var
  | Prop
  | Arrow of ty * ty
  | Tyop of tyop * ty list

type 'b ty_binder = (ty,'b) binder

type pty =
  | Ty of ty
  | ForallK of pty ty_binder

type term =
  | Var of term var
  | Abs of ty * term term_binder
  | App of term * term
  | Impl of term * term
  | Forall of ty * term term_binder
  | AbsT of term ty_binder

and 'b term_binder = (term, 'b) binder

type pterm =
  | Term of term
  | ForallT of pterm ty_binder

type context =
  { ty:ty var list;
    var: (term var * ty) list
  }


type proof_context =
  {
    ctx:context;
    hyp:term list
  }


type proof =
  { hyp:context;
    thm:term;
    rule:rule;
  }

and rule =
  | Assume
  | ImplE of proof * proof
  | ImplI of proof
  | ForallE of proof * term
  | ForallI of proof
  | ForallTE of proof * ty
  | ForallTI of proof

type const_id

type arity

type decl =
  | ConstDecl of const_id * pty
  | ConstDef of const_id * pty * pterm
  | TyopDef of tyop * arity

type ast = decl list

val equal_ty    : ty -> ty -> bool
val equal_pty   : pty -> pty -> bool
val equal_term  : term -> term -> bool
val equal_pterm : pterm -> pterm -> bool

val pp_ty : Format.formatter -> ty -> unit

val pp_pty : Format.formatter -> pty -> unit

val pp_term : Format.formatter -> term -> unit

val pp_pterm : Format.formatter -> pterm -> unit

val empty_context : context

val arity_of_int : int -> arity

val const_id_of_string : string -> const_id

val tyop_of_string : string -> tyop

val mkfree_tyvar : ty var -> ty

val mkfree_var : term var -> term

val newvar_ty : string -> ty var

val prop : ty bindbox

val arrow : ty bindbox -> ty bindbox -> ty bindbox

val tyop : tyop -> ty bindbox list -> ty bindbox

val ty : ty bindbox -> pty bindbox

val forallK : ty var -> pty bindbox -> pty bindbox

val newvar : string -> term var

val abs : term var -> ty bindbox -> term bindbox -> term bindbox

val app : term bindbox -> term bindbox -> term bindbox

val impl : term bindbox -> term bindbox -> term bindbox

val forall : term var -> ty bindbox -> term bindbox -> term bindbox

val absT : ty var -> term bindbox -> term bindbox

val term : term bindbox -> pterm bindbox

val forallT : ty var -> pterm bindbox -> pterm bindbox
