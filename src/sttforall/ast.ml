open Bindlib

type tyop = string
[@@deriving show, eq]

let pp_var _ fmt var = Format.fprintf fmt "%s" (name_of var)

let pp_binder mkfree _ pp fmt binder =
  let var,term = unbind mkfree binder in
  let var_name = name_of var in
  Format.fprintf fmt "\\%s. %a" var_name pp term

let equal_var f x y = eq_vars x y

let equal_binder mkfree _ fb x y =
  let var = free_of (new_var mkfree "dummy") in
  fb (subst x var) (subst y var)

type ty =
  | TyVar of ty var
  | Prop
  | Arrow of ty * ty
  | Tyop of tyop * ty var list
[@@deriving show, eq]

type 'b ty_binder = (ty,'b) binder

let pp_ty_binder pp' fmt binder = pp_binder (fun v -> TyVar v) (fun x -> assert false) pp' fmt binder
let equal_ty_binder fb x y = equal_binder (fun v -> TyVar v) (fun x -> assert false) fb x y

type pty =
  | Ty of ty
  | ForallK of pty ty_binder
[@@deriving show, eq]


type term =
  | Var of term var
  | Abs of ty * term term_binder
  | App of term * term
  | Impl of term * term
  | Forall of ty * term term_binder
  | AbsT of term ty_binder
[@@deriving show]

and 'b term_binder = (term, 'b) binder
      [@polyprinter pp_binder (fun v -> Var v)]

let rec equal_term x y =
  match x, y with
  | Var x, Var y -> eq_vars x y
  | Abs(ty,bx), Abs(ty',by)
  | Forall(ty,bx), Forall(ty',by) ->
    if equal_ty ty ty' then
      let var = Var (new_var (fun x -> Var x) "dummy") in
      equal_term (subst bx var) (subst by var)
    else
      false
  | App(f,a), App(f',a')
  | Impl(f,a),Impl(f',a') ->
    equal_term f f' && equal_term a a'
  | AbsT(bx), AbsT(by) -> equal_ty_binder (fun x -> assert false) bx by
  | _ -> false

let equal_term_binder _ bx by =
    let var = Var (new_var (fun x -> Var x) "dummy") in
    equal_term (subst bx var) (subst by var)

type pterm =
  | Term of term
  | ForallT of pterm ty_binder
[@@deriving show, eq]

type context =
  { ty:ty var list;
    var: (term var * ty) list
  }
[@@deriving show, eq]

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
[@@deriving show, eq]

and rule =
  | Assume
  | ImplE of proof * proof
  | ImplI of proof
  | ForallE of proof * term
  | ForallI of proof
  | ForallTE of proof * ty
  | ForallTI of proof
[@@deriving show, eq]

let mkfree_tyvar = fun x -> TyVar x

let mkfree_var = fun x -> Var x

let var_ty = fun x -> new_var mkfree_tyvar x

let prop = box Prop

let arrow tyl tyr = box_apply2 (fun tyl tyr -> Arrow(tyl,tyr)) tyl tyr

let tyop op vars = box (Tyop(op,vars))

let ty x = box_apply (fun b -> Ty(b)) x

let forallK var_ty pty = box_apply (fun b -> ForallK b) (bind_var var_ty pty)

let var = fun x -> new_var mkfree_var x

let abs var ty t = box_apply (fun b -> Abs(unbox ty,b)) (bind_var var t)

let app f a = box_apply2 (fun f a -> App(f,a)) f a

let impl f a = box_apply2 (fun f a -> Impl(f,a)) f a

let forall var ty t = box_apply (fun b -> Forall(unbox ty,b)) (bind_var var t)

let absT var_ty t = box_apply (fun b -> AbsT(b)) (bind_var var_ty t)

let term t = box_apply (fun b -> Term b) t

let forallT var_ty t = box_apply (fun b -> ForallT b) (bind_var var_ty t)
