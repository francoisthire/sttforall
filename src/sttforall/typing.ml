open Bindlib
open Ast

module TyVarSet = Set.Make (struct type t = ty var let compare = compare end)

module ConstMap = Map.Make (struct type t = const_id let compare = compare end)

module TyopMap = Map.Make (struct type t = tyop let compare = compare end)

type signature =
  {
    tyop : arity TyopMap.t;
    typeof : pty ConstMap.t;
    defof : (pterm option) ConstMap.t
  }

let rec ty_fv ty =
  match ty with
  | TyVar x -> TyVarSet.singleton x
  | Prop -> TyVarSet.empty
  | Arrow(tyl, tyr) -> TyVarSet.union (ty_fv tyl) (ty_fv tyr)
  | Tyop(_, vars) -> List.fold_left (fun s ty -> TyVarSet.union s (ty_fv ty)) TyVarSet.empty vars

let rec pty_fv pty =
  match pty with
  | Ty ty -> ty_fv ty
  | ForallK bdty ->
    let (var, pty) = unbind mkfree_tyvar  bdty in
    TyVarSet.remove var (pty_fv pty)

let wf_ctx ctx =
  List.for_all (fun (var,ty) ->
      TyVarSet.for_all (fun tyvar -> List.mem tyvar ctx.ty) (ty_fv ty)) ctx.var

let add_binding ctx x ty = {ctx with var = (x,ty)::ctx.var}

type typing_error =
  | VariableNotFound of context * term var
  | WrongArgumentType of context * term * ty
  | ArrowExpected of context * term * ty
  | PropExpected of context * pterm
  | TyopDeclaredTwice of tyop
  | ConstantDeclaredTwice of const_id
  | WrongConstantType of const_id * pty * pterm

exception TypingError of typing_error

let rec infer ctx term =
  match term with
  | Var x ->
    begin
      try
        snd @@ List.find (fun (var,ty) -> eq_vars x var) ctx.var
      with Not_found ->
        raise @@ TypingError (VariableNotFound(ctx,x))
    end
  | Abs(ty,binder) ->
    let var, term = unbind mkfree_var binder in
    let ctx' = {ctx with var = (var,ty)::ctx.var} in
    infer ctx' term
  | App(f,a) ->
    let ty = infer ctx f in
    begin
      match ty with
      | Arrow(tyl,tyr) ->
        if has_type ctx a tyl then
          tyr
        else
          raise @@ TypingError (WrongArgumentType(ctx,a,tyl))
      | _ -> raise @@ TypingError (ArrowExpected(ctx,f,ty))
    end
  | _ -> failwith "todo"

and has_type ctx term ty =
  match term with
  | Var x ->
    wf_ctx ctx && List.mem (x,ty) ctx.var
  | Abs(tyl, binder) ->
    begin
      match ty with
      | Arrow(tyl',tyr') when equal_ty tyl tyl' ->
        let var, term = unbind mkfree_var binder in
        has_type (add_binding ctx var tyl) term tyr'
      | _ -> false
    end
  | App(f,a) ->
    let ty = infer ctx f in
    begin
      match ty with
      | Arrow(tyl,tyr) -> has_type ctx a tyl && equal_ty ty tyr
      | _ -> false
    end
  | Impl(f,a) ->
    let prop = Prop in
    equal_ty prop ty && has_type ctx f prop && has_type ctx a prop
  | Forall(ty, binder) ->
    if has_type ctx term (Prop) then
      let var,term = unbind mkfree_var binder in
      let ctx' = {ctx with var = (var,ty)::ctx.var} in
      equal_ty (infer ctx' term) (Prop)
    else
      false
  | AbsT(binder) -> false

let rec has_ptype ctx term pty =
  match term with
  | AbsT(abs) ->
    begin
      match pty with
      | ForallK(forall) ->
        let dummy = (new_var mkfree_tyvar "dummy") in
        let dummy' = mkfree_tyvar dummy in
        has_ptype {ctx with ty = dummy::ctx.ty} (subst abs dummy') (subst forall dummy')
      | _ -> false
    end
  | _ ->
    match pty with
    | Ty ty -> has_type ctx term ty
    | _ -> false

let rec pinfer ctx pterm =
  match pterm with
  | Term term -> infer ctx term
  | ForallT pbinder ->
    let prop = Prop in
    if phas_type ctx pterm prop then
      prop
    else
      raise @@ TypingError (PropExpected(ctx, pterm))

and phas_type ctx pterm ty =
  match pterm with
  | Term term -> has_type ctx term ty
  | ForallT pbinder ->
    let var,pterm = unbind mkfree_tyvar pbinder in
    let prop = Prop in
    let ctx' = {ctx with ty = var::ctx.ty} in
    phas_type ctx' pterm prop && equal_ty ty prop

let phas_ptype ctx pterm pty =
  match pterm with
  | Term term -> has_ptype ctx term pty
  | ForallT term ->
    match pty with
    | Ty ty -> phas_type ctx pterm ty
    | _ -> false



let empty_signature = {tyop = TyopMap.empty; typeof = ConstMap.empty ; defof = ConstMap.empty}

let arity_of sg tyop =
  if TyopMap.mem tyop sg.tyop then
    Some (TyopMap.find tyop sg.tyop)
  else
    None

let add_tyop sg tyop arity =
  if TyopMap.mem tyop sg.tyop then
    raise @@ TypingError(TyopDeclaredTwice tyop)
  else
    {sg with tyop = TyopMap.add tyop arity sg.tyop}

let add_const sg const pty mpte : signature =
  if ConstMap.mem const sg.typeof then
    raise @@ TypingError(ConstantDeclaredTwice const)
  else
    match mpte with
    | None -> {sg with typeof = ConstMap.add const pty sg.typeof;
               defof = ConstMap.add const None sg.defof}
    | Some pte ->
      if phas_ptype Ast.empty_context pte pty then
        {sg
         with typeof = ConstMap.add const pty sg.typeof;
              defof = ConstMap.add const (Some pte) sg.defof}
      else
        raise @@ TypingError(WrongConstantType(const, pty, pte))

let check_decl sg decl =
  match decl with
  | Ast.ConstDecl (c,pty) ->
    begin
      try
        Ok (add_const sg c pty None)
      with TypingError e -> Error e
    end
  | Ast.ConstDef (c,pty,pte) ->
    begin
      try
        Ok (add_const sg c pty (Some pte))
      with TypingError e -> Error e
    end
  | Ast.TyopDef (tyop,arity) ->
    begin
      try
        Ok (add_tyop sg tyop arity)
      with TypingError e -> Error e
    end

let check_ast sg ast =
  let bind result decl =
    match result with
    | Ok sg -> check_decl sg decl
    | Error e -> Error e
  in
  List.fold_left bind (Ok sg) ast
