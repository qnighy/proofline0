open Term
open Format

module Names = Map.Make (
    struct 
      type t = string
      let compare = Pervasives.compare
    end)

type environment = {
  defsize : int;
  deflist : (string * term * term option) list;
  nearest_names : int Names.t
}

let empty_env = {
  defsize = 0;
  deflist = [];
  nearest_names = Names.empty
}

let rec env_add_num env v n t1 t2 =
  let vn = v ^ (string_of_int n) in
  if not (List.exists (function (x,_,_) -> x = vn) env.deflist) then
    {
      defsize = env.defsize + 1;
      deflist = (vn,t1,t2)::env.deflist;
      nearest_names =
        if v = "_" then env.nearest_names
        else
          Names.add v env.defsize env.nearest_names
    }
  else
    env_add_num env v (n+1) t1 t2

let env_add env v t1 t2 =
  if not (List.exists (function (x,_,_) -> x = v) env.deflist) then
    {
      defsize = env.defsize + 1;
      deflist = (v,t1,t2)::env.deflist;
      nearest_names =
        if v = "_" then env.nearest_names
        else
          Names.add v env.defsize env.nearest_names
    }
  else
    env_add_num env v 0 t1 t2

let rec convert_debrujin env = function
  | TermAstApply (t1,t2) ->
      TermApply (convert_debrujin env t1, convert_debrujin env t2)
  | TermAstForall (v,t1,t2,l) ->
      let ct1 = convert_debrujin env t1 in
      TermForall (v, ct1,
        convert_debrujin (env_add env v ct1 None) t2, l)
  | TermAstFun (v,t1,t2,l) ->
      let ct1 = convert_debrujin env t1 in
      TermFun (v, ct1,
        convert_debrujin (env_add env v ct1 None) t2, l)
  | TermAstLetIn (v,t1,t2,t3,l) ->
      let ct1 = convert_debrujin env t1 in
      let ct2 = convert_debrujin env t2 in
      TermLetIn (v, ct1, ct2,
        convert_debrujin (env_add env v ct1 (Some ct2)) t3, l)
  | TermAstVarRef vn ->
      (try
        TermVarRef (env.defsize - 1 - (Names.find vn env.nearest_names))
      with Not_found ->
        failwith (Misc.sprintf "variable or definition \"%s\" not found" vn))
  | TermAstSort u -> TermSort u

let rec pp_print_term env ppf = function
  | TermApply (t1,t2) ->
      fprintf ppf "(%a %a)"
        (pp_print_term env) t1
        (pp_print_term env) t2
  | TermForall (v,t1,t2,false) ->
      let new_env = env_add env v t1 None in
      fprintf ppf "(forall %s:%a, %a)"
        (let (v0,_,_) = List.nth new_env.deflist 0 in v0)
        (pp_print_term env) t1
        (pp_print_term new_env) t2
  | TermForall (_,t1,t2,true) ->
      let new_env = env_add env "_" t1 None in
      fprintf ppf "%a -%s %a"
        (pp_print_term env) t1
        "@"
        (pp_print_term new_env) t2
  | TermFun (v,t1,t2,false) ->
      let new_env = env_add env v t1 None in
      fprintf ppf "(fun %s:%a => %a)"
        (let (v0,_,_) = List.nth new_env.deflist 0 in v0)
        (pp_print_term env) t1
        (pp_print_term new_env) t2
  | TermFun (v,t1,t2,true) ->
      let new_env = env_add env v t1 None in
      fprintf ppf "(fun *%s:%a => %a)"
        (let (v0,_,_) = List.nth new_env.deflist 0 in v0)
        (pp_print_term env) t1
        (pp_print_term new_env) t2
  | TermLetIn (v,t1,t2,t3,false) ->
      let new_env = env_add env v t1 (Some t2) in
      fprintf ppf "(let %s:%a := %a in %a)"
        (let (v0,_,_) = List.nth new_env.deflist 0 in v0)
        (pp_print_term env) t1
        (pp_print_term env) t2
        (pp_print_term new_env) t3
  | TermLetIn (v,t1,t2,t3,true) ->
      let new_env = env_add env v t1 (Some t2) in
      fprintf ppf "(let *%s:%a := %a in %a)"
        (let (v0,_,_) = List.nth new_env.deflist 0 in v0)
        (pp_print_term env) t1
        (pp_print_term env) t2
        (pp_print_term new_env) t3
  | TermVarRef x ->
      fprintf ppf "%s"
        (let (v0,_,_) = List.nth env.deflist x in v0)
  | TermSort u ->
      fprintf ppf "Type(%d)" u

let rec pp_print_term_literal ppf = function
  | TermApply (t1,t2) ->
      fprintf ppf "(%a %a)"
        pp_print_term_literal t1
        pp_print_term_literal t2
  | TermForall (v,t1,t2,false) ->
      fprintf ppf "(forall :%a, %a)"
        pp_print_term_literal t1
        pp_print_term_literal t2
  | TermForall (_,t1,t2,true) ->
      fprintf ppf "%a -%s %a"
        pp_print_term_literal t1
        "@"
        pp_print_term_literal t2
  | TermFun (v,t1,t2,false) ->
      fprintf ppf "(fun :%a => %a)"
        pp_print_term_literal t1
        pp_print_term_literal t2
  | TermFun (v,t1,t2,true) ->
      fprintf ppf "(fun *:%a => %a)"
        pp_print_term_literal t1
        pp_print_term_literal t2
  | TermLetIn (v,t1,t2,t3,false) ->
      fprintf ppf "(let :%a := %a in %a)"
        pp_print_term_literal t1
        pp_print_term_literal t2
        pp_print_term_literal t3
  | TermLetIn (v,t1,t2,t3,true) ->
      fprintf ppf "(let *:%a := %a in %a)"
        pp_print_term_literal t1
        pp_print_term_literal t2
        pp_print_term_literal t3
  | TermVarRef x ->
      fprintf ppf "[%d]" x
  | TermSort u ->
      fprintf ppf "Type(%d)" u

let rec shift_term rk d t =
  match t with
  | TermApply (t1,t2) ->
      TermApply (shift_term rk d t1, shift_term rk d t2)
  | TermForall (v,t1,t2,l) ->
      TermForall (v, shift_term rk d t1, shift_term rk (d+1) t2, l)
  | TermFun (v,t1,t2,l) ->
      TermFun (v, shift_term rk d t1, shift_term rk (d+1) t2, l)
  | TermLetIn (v,t1,t2,t3,l) ->
      TermLetIn (v, shift_term rk d t1, shift_term rk d t2,
      shift_term rk (d+1) t3, l)
  | TermVarRef x when x >= d -> TermVarRef (x+rk)
  | TermVarRef x -> t
  | TermSort u -> t

let rec subst_term rk t s =
  match t with
  | TermApply (t1,t2) ->
      TermApply (subst_term rk t1 s, subst_term rk t2 s)
  | TermForall (v,t1,t2,l) ->
      TermForall (v, subst_term rk t1 s, subst_term (rk+1) t2 s, l)
  | TermFun (v,t1,t2,l) ->
      TermFun (v, subst_term rk t1 s, subst_term (rk+1) t2 s, l)
  | TermLetIn (v,t1,t2,t3,l) ->
      TermLetIn (v, subst_term rk t1 s, subst_term rk t2 s,
      subst_term (rk+1) t3 s, l)
  | TermVarRef x when x = rk -> shift_term rk 0 s
  | TermVarRef x when x > rk -> TermVarRef (x-1)
  | TermVarRef x -> t
  | TermSort u -> t

let rec reduce env t =
  match t with
  | TermApply (t1,t2) ->
      (match reduce env t1 with
      | TermApply (t1a,t1b) -> t
      | TermForall _ -> assert false
      | TermFun (v,t1a,t1b,l) -> reduce env (subst_term 0 t1b t2)
      | TermLetIn _ -> assert false
      | TermVarRef _ -> t
      | TermSort _ -> assert false)
  | TermForall _ -> t
  | TermFun _ -> t
  | TermLetIn (v,t1,t2,t3,l) -> reduce env (subst_term 0 t3 t2)
  | TermVarRef x -> (
      match List.nth env.deflist x with
      | (_,_,None) -> t
      | (_,_,Some s) -> reduce env (shift_term (x+1) 0 s))
  | TermSort _ -> t

let rec check_cast env t1 t2 =
  match reduce env t1,reduce env t2 with
  | TermApply (t1a,t1b),TermApply (t2a,t2b) ->
      check_cast env t1a t2a;
      check_cast env t1b t2b
  | TermForall (v1,t1a,t1b,l1),TermForall (v2,t2a,t2b,l2) when l1 = l2 ->
      check_cast env t2a t1a;
      check_cast (env_add env v1 t1a None) t1b t2b
  | TermVarRef x1,TermVarRef x2 when x1 = x2 -> ()
  | TermSort u1,TermSort u2 when u1 <= u2 -> ()
  | _,_ -> failwith (
      Misc.sprintf "failed to cast from %a to %a"
      (pp_print_term env) t1
      (pp_print_term env) t2
    )

let rec check_sort env t =
  match reduce env t with
  | TermSort _ -> ()
  | _ -> failwith (
      Misc.sprintf "failed to cast from %a to Type(_)"
      (pp_print_term env) t
    )

let rec check_type env occ ldep t =
  match t with
  | TermApply (t1,t2) ->
      let t1t = check_type env occ ldep t1 in
      (match reduce env t1t with
      | TermForall (v,tt1,tt2,l) ->
          let t2t = check_type env occ
            (if l then ldep else 0) t2 in
          check_cast env tt1 t2t;
          subst_term 0 tt2 t2
      | _ ->
        failwith (Misc.sprintf "%a has type %a but expected product"
          (pp_print_term env) t1
          (pp_print_term env) t1t)
        )
  | TermForall (v,t1,t2,l) ->
      let new_occ = (if l then Some (ref false) else None)::occ in
      (match reduce env (check_type env occ ldep t1),
          reduce env (check_type (env_add env v t1 None)
              new_occ (ldep+1) t2) with
      | TermSort u1,TermSort 0 -> TermSort 0
      | TermSort u1,TermSort u2 -> TermSort (max u1 u2)
      | _,_ -> assert false)
  | TermFun (v,t1,t2,l) ->
      check_sort env (check_type env occ ldep t1);
      let new_occ = (if l then Some (ref false) else None)::occ in
      let new_env = env_add env v t1 None in
      let r = TermForall
          (v,t1,check_type new_env new_occ (ldep+1) t2,l) in
      (match check_type env occ ldep r with
      | TermSort _ -> ()
      | _ -> assert false
      );
      (match List.hd new_occ with
      | None -> ()
      | Some lref ->
          if !lref then
            ()
          else
            failwith (Misc.sprintf "%a is linear but unused"
              (pp_print_term new_env) (TermVarRef 0)
            )
      );
      r
  | TermLetIn (v,t1,t2,t3,l) ->
      let new_occ = (if l then Some (ref false) else None)::occ in
      check_sort env (check_type env occ ldep t1);
      check_cast env (check_type env occ (if l then ldep else 0) t2) t1;
      check_type (env_add env v t1 (Some t2)) new_occ (ldep+1) t3
  | TermVarRef x ->
      let l = try List.nth occ x with Failure _ -> None in
      (match l with
      | None -> ()
      | Some lref ->
          if x >= ldep then
            failwith (Misc.sprintf "%a is linear but it's nonlinear context"
              (pp_print_term env) t
            )
          else if !lref then
            failwith (Misc.sprintf "%a is linear and already used"
              (pp_print_term env) t
            )
          else
            lref := true
      );
      (match List.nth env.deflist x with
      | (_,tt,_) -> shift_term (x+1) 0 tt)
  | TermSort u -> TermSort (u+1)

