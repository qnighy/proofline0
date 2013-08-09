open Format

type term_ast =
  | TermAstApply of  term_ast * term_ast
  | TermAstForall of  string * term_ast * term_ast * bool
  | TermAstFun of  string * term_ast * term_ast * bool
  | TermAstLetIn of  string * term_ast * term_ast * term_ast * bool
  | TermAstVarRef of string
  | TermAstSort of int

let rec pp_print_term_ast ppf = function
  | TermAstApply (t1,t2) ->
      fprintf ppf "(%a %a)"
        pp_print_term_ast t1
        pp_print_term_ast t2
  | TermAstForall (v,t1,t2,false) ->
      fprintf ppf "(forall %s:%a, %a)"
        v
        pp_print_term_ast t1
        pp_print_term_ast t2
  | TermAstForall (_,t1,t2,true) ->
      fprintf ppf "(%a %s %a)"
        pp_print_term_ast t1
        "-@"
        pp_print_term_ast t2
  | TermAstFun (v,t1,t2,false) ->
      fprintf ppf "(fun %s:%a => %a)"
        v
        pp_print_term_ast t1
        pp_print_term_ast t2
  | TermAstFun (v,t1,t2,true) ->
      fprintf ppf "(fun *%s:%a => %a)"
        v
        pp_print_term_ast t1
        pp_print_term_ast t2
  | TermAstLetIn (v,t1,t2,t3,false) ->
      fprintf ppf "(let %s:%a := %a in %a)"
        v
        pp_print_term_ast t1
        pp_print_term_ast t2
        pp_print_term_ast t3
  | TermAstLetIn (v,t1,t2,t3,true) ->
      fprintf ppf "(let *%s:%a := %a in %a)"
        v
        pp_print_term_ast t1
        pp_print_term_ast t2
        pp_print_term_ast t3
  | TermAstVarRef v ->
      fprintf ppf "%s" v
  | TermAstSort u ->
      fprintf ppf "Type(%d)" u

type term =
  | TermApply of  term * term
  | TermForall of  string * term * term * bool
  | TermFun of  string * term * term * bool
  | TermLetIn of  string * term * term * term * bool
  | TermVarRef of int
  | TermSort of int

