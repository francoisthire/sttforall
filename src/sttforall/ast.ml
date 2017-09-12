open Bindlib

type tyop = string
[@@deriving show, eq]

let pp_var _ fmt var = Format.fprintf fmt "%s" (name_of var)

let pp_binder _ pp fmt binder =
  let var,term = unbind free_of binder in
  let var_name = name_of var in
  Format.fprintf fmt "\\%s. %a" var_name pp term

let equal_var f x y = eq_vars x y

let equal_binder _ fb x y =
  let var,binder = unbind free_of x in
  let var' = free_of var in
  fb (subst x var') (subst y var')

type ty =
  | TyVar of ty var
  | Prop
  | Arrow of ty * ty
  | Tyop of tyop * ty var list
[@@deriving show, eq]

type pty =
  | Ty of ty
  | ForallK of (ty, pty) binder
[@@deriving show, eq]

type term =
  | Var of term var
  | Abs of ty * (term, term) binder
  | App of term * term
  | Impl of term * term
  | Forall of ty * (term, term) binder
  | AbsT of (ty, term) binder
[@@deriving show, eq]

type pterm =
  | Term of term
  | ForallT of (ty, pterm) binder
[@@deriving show, eq]

type context =
  { ty:ty list;
    var: (term var * ty) list
  }
[@@deriving show]

type proof_context =
  {
    ctx:context;
    hyp:term list
  }
[@@deriving show]

type proof =
  { hyp:context;
    thm:term;
    rule:rule;
  }
[@@deriving show]

and rule =
  | Assume
  | ImplE of proof * proof
  | ImplI of proof
  | ForallE of proof * term
  | ForallI of proof
  | ForallTE of proof * ty
  | ForallTI of proof
[@@deriving show]

let mkfree_tyvar = fun x -> TyVar x

let mkfree_var = fun x -> Var x

let var_ty = fun x -> new_var mkfree_tyvar x

let prop = box Prop

let arrow tyl tyr = box_apply2 (fun tyl tyr -> Arrow(tyl,tyr)) tyl tyr

let tyop op vars = box (Tyop(op,vars))

let ty x = box_apply (fun b -> Ty(b)) x

let var = fun x -> new_var mkfree_var x

let forallK var_ty pty = box_apply (fun b -> ForallK b) (bind_var var_ty pty)

let abs var ty t = box_apply (fun b -> Abs(ty,b)) (bind_var var t)

let abs var ty t = box_apply (fun b -> Forall(ty,b)) (bind_var var t)

let absT var_ty t = box_apply (fun b -> AbsT(b)) (bind_var var_ty t)

let forallT var_ty t = box_apply (fun b -> ForallT b) (bind_var var_ty t)
