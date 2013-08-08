open Format

open Term
open Parser
open Lexer
open Environment

let parse_term str =
  convert_debrujin 0 (Names.empty)
    (Parser.main Lexer.token (Lexing.from_string str))

let parse_and_print str =
  printf "%a\n" pp_print_term_ast
    (Parser.main Lexer.token (Lexing.from_string str))

let print_term env =
  printf "%a\n" (pp_print_term env)
let print_type env t =
  print_term env (check_type env t)

let parse_print_term str =
  print_term {deflist = []} (parse_term str);
  print_type {deflist = []} (parse_term str)

let () =
  parse_and_print "fun x:Type(0) => x fun y:x => x y z";
  parse_and_print "fun x:Type(0) => x forall y:x, x y z";
  parse_and_print "forall x:Type(0), x fun y:x => x y z";
  parse_and_print "forall x:Type(0), x forall y:x, x y z";
  parse_and_print "x -@ y -@ z";
  parse_and_print "x -@ y -> z";
  parse_and_print "x -> y -@ z";
  parse_and_print "x -> y -> z";
  parse_and_print "let x : y := z in t";
  let env = { deflist = [] } in
  print_term env (TermSort 0);
  print_type env (TermSort 0);
  print_term env (TermForall ("A", TermSort 0, TermVarRef 0));
  print_type env (TermForall ("A", TermSort 0, TermVarRef 0));
  print_term env (TermFun ("A", TermSort 0, TermVarRef 0));
  print_type env (TermFun ("A", TermSort 0, TermVarRef 0));
  print_term env (TermFun ("A", TermSort 0, TermFun ("A", TermSort 0, TermVarRef 0)));
  print_type env (TermFun ("A", TermSort 0, TermFun ("A", TermSort 0, TermVarRef 0)));
  parse_print_term "forall A:Type(0), forall B:Type(0), A -> B";
  parse_print_term "forall A:Type(0), forall B:A->Type(0), forall x:A, B x";
  parse_print_term "fun A:Type(1)=> let x:(A->A):=(fun y:A=>y) in A"

