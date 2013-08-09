open Format

open Term
open Parser
open Lexer
open Environment
open Instruction

let rec nextInstruction lexbuf env =
  match Parser.instruction Lexer.token lexbuf with
  | EndOfInstruction -> Format.eprintf "Successfully loaded <stdin>.\n"
  | DefinitionInstruction (v,t1,t2) ->
      let t1 = convert_debrujin env t1 in
      let t2 = convert_debrujin env t2 in
      let t1t = check_type env [] 0 t1 in
      check_sort env t1t;
      let t2t = check_type env [] 0 t2 in
      check_cast env t2t t1;
      Format.eprintf "Defined %s.\n" v;
      nextInstruction lexbuf (env_add env v t1 (Some t2))
  | AxiomInstruction (v,t1) ->
      let t1 = convert_debrujin env t1 in
      let t1t = check_type env [] 0 t1 in
      check_sort env t1t;
      Format.eprintf "Assumed %s.\n" v;
      nextInstruction lexbuf (env_add env v t1 None)

let () =
  let lexbuf = Lexing.from_channel stdin in
  nextInstruction lexbuf empty_env

