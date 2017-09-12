open Bindlib
open Ast

module TyVarSet = Set.Make (struct type t = ty var let compare = compare end)

let rec ty_fv ty =
  match ty with
  | TyVar x -> TyVarSet.singleton x
  | Prop -> TyVarSet.empty
  | Arrow(tyl, tyr) -> TyVarSet.union (ty_fv tyl) (ty_fv tyr)
  | Tyop(_, tys) -> List.fold_left (fun s ty -> TyVarSet.union s (ty_fv ty)) tys

let rec pty_fv pty =
  match pty with
  | Ty ty -> ty_fv ty
  | ForallK(var, pty) -> TyVarSet.remove (pty_fv pty) var

let eq_ty ty ty' = failwith "todo"

let wf_ctx ctx =
  List.for_all (fun (var,ty) ->
      TyVarSet.for_all (fun tyvar -> List.mem tyvar ctx.ty) (ty_fv ty)) ctx.var

let add_binding ctx x ty = {ctx with var = (x,ty)::var}

type typing_error =
  | VariableNotFound of ctx * term var

exception Typing_error of typing_error

let infer ctx term =
  match term with
  | Var x ->
    begin
      try
        snd @@ List.find (fun (var,ty) -> eq_vars x var) ctx.var
      with Not_found ->
        raise Typing_error (VariableNotFound(ctx,x))
    end
  |


let has_type ctx term ty =
  match term with
  | Var x ->
    wf_ctx ctx && List.mem (x,ty) ctx.var
  | Abs(tyl, binder) ->
    begin
      match ty with
      | Arrow(tyl',tyr') when eq_ty tyl tyl' ->
        let var, term = unbind (fun x -> Var x) binder in
        has_type (add_binding ctx var tyl) term tyr'
      | _ -> false
    end
  | App(f,a) -> failwith "todo"
  | _ -> failwith "todo"
