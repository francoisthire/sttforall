open Ast
open Bindlib
open Parser

module EP = EParser

type ty_env = (string * ty var) list

type term_env = (string * term var) list

type scoping_error =
  | FreeTyVariable of string
  | FreeVariable of string

exception ScopingError of scoping_error

let rec tybox_of_ty (env:ty_env) (ty:EP.p_ty) : ty bindbox =
  match ty with
  | EP.P_VarTy(s) ->
    begin
      try
        let var = List.assoc s env in
        box_of_var var
      with Not_found -> raise @@ ScopingError(FreeTyVariable s)
    end
  | EP.P_Prop -> prop
  | EP.P_Arrow(l,r) -> arrow (tybox_of_ty env l) (tybox_of_ty env r)
  | EP.P_Tyop(s,tyl) ->
    let tyl' = List.map (tybox_of_ty env) tyl in
    let tyop = tyop_of_string s in
    Ast.tyop tyop tyl'

let rec ptybox_of_pty (env:ty_env) (pty:EP.p_pty) : pty bindbox =
  match pty with
  | EP.P_Ty ty -> Ast.ty (tybox_of_ty env ty)
  | EP.P_ForallK(s,pty) ->
    let var = newvar_ty s in
    let pty' = ptybox_of_pty ((s,var)::env) pty in
    forallK var pty'

let rec tebox_of_te (tyenv:ty_env) (teenv:term_env) (te:EP.p_term) : term bindbox =
  match te with
  | EP.P_Var(s) ->
    begin
      try
        let var = List.assoc s teenv in
        box_of_var var
      with Not_found -> raise @@ ScopingError(FreeVariable s)
    end
  | EP.P_Abs(s, ty, te) ->
    let var = newvar s in
    let te' = tebox_of_te tyenv ((s,var)::teenv) te in
    let ty' = tybox_of_ty tyenv ty in
    abs var ty' te'
  | EP.P_App(tl,tr) ->
    let tl' = tebox_of_te tyenv teenv tl in
    let tr' = tebox_of_te tyenv teenv tr in
    app tl' tr'
  | EP.P_Forall(s,ty,te) ->
    let var = newvar s in
    let te' = tebox_of_te tyenv ((s,var)::teenv) te in
    let ty' = tybox_of_ty tyenv ty in
    forall var ty' te'
  | EP.P_Impl(tl,tr) ->
    let tl' = tebox_of_te tyenv teenv tl in
    let tr' = tebox_of_te tyenv teenv tr in
    impl tl' tr'
  | EP.P_AbsT(s, te) ->
    let var = newvar_ty s in
    let te' = tebox_of_te ((s,var)::tyenv) teenv te in
    absT var te'

let rec ptebox_of_pte (tyenv:ty_env) (teenv:term_env) (pte:EP.p_pterm) : pterm bindbox =
  match pte with
  | EP.P_Term(te) ->
    let te' = tebox_of_te tyenv teenv te in
    term te'
  | EP.P_ForallT(s, pte) ->
    let var = newvar_ty s in
    let pte'  = ptebox_of_pte ((s,var)::tyenv) teenv pte in
    forallT var pte'

let to_pty pty = unbox (ptybox_of_pty [] pty)

let to_pte pte = unbox (ptebox_of_pte [] [] pte)

let to_ast ast =
  match ast with
  | EP.P_ConstDecl(s, pty) ->
    ConstDecl(const_id_of_string s, to_pty pty)
  | EP.P_ConstDef(s, pty, pte) ->
    ConstDef(const_id_of_string s, to_pty pty, to_pte pte)
  | EP.P_TyopDef(s,i) ->
    let a = arity_of_int i in
    let id = tyop_of_string s in
    TyopDef(id,a)
