open Term
open Format

type environment = {
  deflist : (string * term * term option) list
}

module Names = Map.Make (
    struct 
      type t = string
      let compare = Pervasives.compare
    end)

let rec convert_debrujin depth dic = function
  | TermAstApply (t1,t2) ->
      TermApply (convert_debrujin depth dic t1, convert_debrujin depth dic t2)
  | TermAstForall (v,t1,t2) ->
      TermForall (v, convert_debrujin depth dic t1,
        convert_debrujin (depth+1) (Names.add v depth dic) t2)
  | TermAstLolli (t1,t2) ->
      TermLolli (convert_debrujin depth dic t1, convert_debrujin depth dic t2)
  | TermAstFun (v,t1,t2) ->
      TermFun (v, convert_debrujin depth dic t1,
        convert_debrujin (depth+1) (Names.add v depth dic) t2)
  | TermAstLetIn (v,t1,t2,t3) ->
      TermLetIn (v, convert_debrujin depth dic t1,
        convert_debrujin depth dic t2,
        convert_debrujin (depth+1) (Names.add v depth dic) t3)
  | TermAstVarRef vn ->
      TermVarRef (depth - 1 - (Names.find vn dic))
  | TermAstSort u -> TermSort u

let rec env_add_num env v n t1 t2 =
  let vn = v ^ (string_of_int n) in
  if not (List.exists (function (x,_,_) -> x = vn) env.deflist) then
    { env with deflist = (vn,t1,t2)::env.deflist }
  else
    env_add_num env v (n+1) t1 t2

let env_add env v t1 t2 =
  if not (List.exists (function (x,_,_) -> x = v) env.deflist) then
    { env with deflist = (v,t1,t2)::env.deflist }
  else
    env_add_num env v 0 t1 t2

let rec pp_print_term env ppf = function
  | TermApply (t1,t2) ->
      fprintf ppf "(%a %a)"
        (pp_print_term env) t1
        (pp_print_term env) t2
  | TermForall (v,t1,t2) ->
      let new_env = env_add env v t1 None in
      fprintf ppf "(forall %s:%a, %a)"
        (let (v0,_,_) = List.nth new_env.deflist 0 in v0)
        (pp_print_term env) t1
        (pp_print_term new_env) t2
  | TermLolli (t1,t2) ->
      fprintf ppf "%a -@ %a"
        (pp_print_term env) t1
        (pp_print_term env) t2
  | TermFun (v,t1,t2) ->
      let new_env = env_add env v t1 None in
      fprintf ppf "(fun %s:%a => %a)"
        (let (v0,_,_) = List.nth new_env.deflist 0 in v0)
        (pp_print_term env) t1
        (pp_print_term new_env) t2
  | TermLetIn (v,t1,t2,t3) ->
      let new_env = env_add env v t1 (Some t2) in
      fprintf ppf "(let %s:%a := %a in %a)"
        (let (v0,_,_) = List.nth new_env.deflist 0 in v0)
        (pp_print_term env) t1
        (pp_print_term env) t2
        (pp_print_term new_env) t3
  | TermVarRef x ->
      fprintf ppf "%s"
        (let (v0,_,_) = List.nth env.deflist x in v0)
  | TermSort u ->
      fprintf ppf "Type(%d)" u

let rec shift_term rk d t =
  match t with
  | TermApply (t1,t2) ->
      TermApply (shift_term rk d t1, shift_term rk d t2)
  | TermForall (v,t1,t2) ->
      TermForall (v, shift_term rk d t1, shift_term rk (d+1) t2)
  | TermLolli (t1,t2) ->
      TermLolli (shift_term rk d t1, shift_term rk d t2)
  | TermFun (v,t1,t2) ->
      TermFun (v, shift_term rk d t1, shift_term rk (d+1) t2)
  | TermLetIn (v,t1,t2,t3) ->
      TermLetIn (v, shift_term rk d t1, shift_term rk d t2,
      shift_term rk (d+1) t3)
  | TermVarRef x when x >= d -> TermVarRef (x+rk)
  | TermVarRef x -> t
  | TermSort u -> t

let rec subst_term rk t s =
  match t with
  | TermApply (t1,t2) ->
      TermApply (subst_term rk t1 s, subst_term rk t2 s)
  | TermForall (v,t1,t2) ->
      TermForall (v, subst_term rk t1 s, subst_term (rk+1) t2 s)
  | TermLolli (t1,t2) ->
      TermLolli (subst_term rk t1 s, subst_term rk t2 s)
  | TermFun (v,t1,t2) ->
      TermFun (v, subst_term rk t1 s, subst_term (rk+1) t2 s)
  | TermLetIn (v,t1,t2,t3) ->
      TermLetIn (v, subst_term rk t1 s, subst_term rk t2 s,
      subst_term (rk+1) t3 s)
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
      | TermLolli _ -> assert false
      | TermFun (v,t1a,t1b) -> reduce env (subst_term 0 t1b t2)
      | TermLetIn _ -> assert false
      | TermVarRef _ -> t
      | TermSort _ -> assert false)
  | TermForall _ -> t
  | TermLolli _ -> t
  | TermFun _ -> t
  | TermLetIn (v,t1,t2,t3) -> reduce env (subst_term 0 t3 t2)
  | TermVarRef x -> (
      match List.nth env.deflist x with
      | (_,_,None) -> t
      | (_,_,Some s) -> reduce env (shift_term (x+1) 0 s))
  | TermSort _ -> t

let rec congruence env t1 t2 =
  match reduce env t1,reduce env t2 with
  | TermApply (t1a,t1b),TermApply (t2a,t2b) ->
      congruence env t1a t2a &&
      congruence env t1b t2b
  | TermForall (v1,t1a,t1b),TermForall (v2,t2a,t2b) ->
      congruence env t1a t2a &&
      congruence (env_add env v1 t1a None) t1b t2b
  | TermLolli (t1a,t1b),TermLolli (t2a,t2b) ->
      congruence env t1a t2a &&
      congruence env t1b t2b
  | TermFun (v1,t1a,t1b),TermFun (v2,t2a,t2b) ->
      congruence env t1a t2a &&
      congruence (env_add env v1 t1a None) t1b t2b
  | TermLetIn (v1,t1a,t1b,t1c),TermLetIn (v2,t2a,t2b,t2c) ->
      congruence env t1a t2a &&
      congruence env t1b t2b &&
      congruence (env_add env v1 t1a None) t1c t2c
  | TermVarRef x1,TermVarRef x2 -> x1 = x2
  | TermSort u1,TermSort u2 -> u1 = u2
  | _,_ -> false

let rec check_type env t =
  match t with
  | TermApply (t1,t2) ->
      (match reduce env (check_type env t1) with
      | TermForall (v,tt1,tt2) ->
          if congruence env tt1 t2 then
            subst_term 0 tt2 t2
          else
            assert false
      | TermLolli (tt1,tt2) ->
          if congruence env tt1 t2 then
            tt2
          else
            assert false
      | _ -> assert false)
  | TermForall (v,t1,t2) ->
      (match reduce env (check_type env t1),
          reduce env (check_type (env_add env v t1 None) t2) with
      | TermSort u1,TermSort 0 -> TermSort 0
      | TermSort u1,TermSort u2 -> TermSort (max u1 u2)
      | _,_ -> assert false)
  | TermLolli (t1,t2) ->
      (match reduce env (check_type env t1),reduce env (check_type env t2) with
      | TermSort u1,TermSort 0 -> TermSort 0
      | TermSort u1,TermSort u2 -> TermSort (max u1 u2)
      | _,_ -> assert false)
  | TermFun (v,t1,t2) ->
      let l = TermForall (v,t1,check_type (env_add env v t1 None) t2) in
      (match check_type env l with
      | TermSort _ -> l
      | _ -> assert false
      )
  | TermLetIn (v,t1,t2,t3) ->
      check_type (env_add env v t1 (Some t2)) t3
  | TermVarRef x ->
      (match List.nth env.deflist x with
      | (_,tt,_) -> tt)
  | TermSort u -> TermSort (u+1)

