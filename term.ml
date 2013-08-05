open Format

type term_ast =
  | TermAstApply of  term_ast * term_ast
  | TermAstForall of  string * term_ast * term_ast
  | TermAstLolli of  term_ast * term_ast
  | TermAstFun of  string * term_ast * term_ast
  | TermAstVarRef of string
  | TermAstSort of int

let rec pp_print_term_ast ppf = function
  | TermAstApply (t1,t2) ->
      fprintf ppf "(%a %a)"
        pp_print_term_ast t1
        pp_print_term_ast t2
  | TermAstForall (v,t1,t2) ->
      fprintf ppf "(forall %s:%a, %a)"
        v
        pp_print_term_ast t1
        pp_print_term_ast t2
  | TermAstLolli (t1,t2) ->
      fprintf ppf "(%a %s %a)"
        pp_print_term_ast t1
        "-@"
        pp_print_term_ast t2
  | TermAstFun (v,t1,t2) ->
      fprintf ppf "(fun %s:%a => %a)"
        v
        pp_print_term_ast t1
        pp_print_term_ast t2
  | TermAstVarRef v ->
      fprintf ppf "%s" v
  | TermAstSort u ->
      fprintf ppf "Type(%d)" u
