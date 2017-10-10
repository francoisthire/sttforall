(* TODO
   - gérer les prioriété dans le parsing des types avec des variants polymorphes
   - gérer les prioriété dans le parsing des termes avec des variants polymorphes
*)

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

module EParser = struct

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

  let reserved_keywords = ["Prop"; "Forall" ; "forall"]

  let check_is_reserved id =
    if List.mem id reserved_keywords then
      Earley.give_up ()
    else
      id

  let parser varTyIdent = id:''[A-Z]+'' -> check_is_reserved id

  let parser tyOpIdent = id:''[a-z][a-zA-Z]*'' -> id

  let parser varIdent = id:''[a-z]+'' -> id

  let parser ident = id:''[a-zA-Z]+'' -> check_is_reserved id

  let parser num = num:''[1-9][0-9]*'' -> num

  let parser ty =
    | x:varTyIdent -> P_VarTy(x)
    | "Prop" -> P_Prop
    | a:ty "->" b:ty -> P_Arrow(a,b)
    | l:tyOpIdent tyl:{t:ty}* -> P_Tyop(l,tyl)
    | "(" t:ty ")" -> t

  let parser pty =
    | x:ty -> P_Ty(x)
    | "Forall" x:varTyIdent "," t:pty -> P_ForallK(x,t)

  let parser term =
    | x:varIdent -> P_Var(x)
    | x:varIdent ":" ty:ty "=>" te:term -> P_Abs(x,ty,te)
    | l:term r:term -> P_App(l,r)
    | l:term "=>" r:term -> P_Impl(l,r)
    | "(" t:term ")" -> t
    | "forall" x:varIdent ":" t:ty "," te:term -> P_Forall(x,t,te)
    | x:varTyIdent "=>" te:term -> P_AbsT(x,te)

  let parser pterm =
    | x:term -> P_Term(x)
    | "Forall" x:varTyIdent "," pte:pterm -> P_ForallT(x, pte)

  let parser toplevel =
    | "#CST" cst:ident ":" a:pty -> P_ConstDecl(cst,a)
    | "#CST" cst:ident ":" a:pty ":=" t:pterm -> P_ConstDef(cst,a,t)
    | "#TYOP" tyop:tyOpIdent  ":" n:num -> P_TyopDef(tyop,int_of_string n)

  let parser full = {l:toplevel "."}*

  let blank buf pos =
    let rec fn state prev ((buf0, pos0) as curr) =
      let open Input in
      let (c, buf1, pos1) = read buf0 pos0 in
      let next = (buf1, pos1) in
      match (state, c) with
      (* Basic blancs. *)
      | (`Ini, ' ' )
      | (`Ini, '\t')
      | (`Ini, '\r')
      | (`Ini, '\n') -> fn `Ini curr next
      (* Comment. *)
      | (`Ini, '/' ) -> fn `Opn curr next
      | (`Opn, '/' ) -> let p = normalize buf1 (line_length buf1) in fn `Ini p p
      (* Other. *)
      | (`Opn, _   ) -> prev
      | (`Ini, _   ) -> curr
    in
    fn `Ini (buf, pos) (buf, pos)

end

let parse_file file : EParser.p_decl list =
  Earley.parse_file EParser.full EParser.blank file
