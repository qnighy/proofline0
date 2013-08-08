type instruction =
  | EndOfInstruction
  | DefinitionInstruction of string * Term.term_ast * Term.term_ast
  | AxiomInstruction of string * Term.term_ast
