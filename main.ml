open Format

open Term
open Parser
open Lexer

let parse_and_print str =
  printf "%a\n" pp_print_term_ast
    (Parser.main Lexer.token (Lexing.from_string str))

let () =
  parse_and_print "fun x:Type(0) => x fun y:x => x y z";
  parse_and_print "fun x:Type(0) => x forall y:x, x y z";
  parse_and_print "forall x:Type(0), x fun y:x => x y z";
  parse_and_print "forall x:Type(0), x forall y:x, x y z";
  parse_and_print "x -@ y -@ z";
  parse_and_print "x -@ y -> z";
  parse_and_print "x -> y -@ z";
  parse_and_print "x -> y -> z";
  parse_and_print "let x : y := z in t"
