%{
  open Term
  open Instruction
%}
%token <string> IDENT
%token <int> INT
%token DEFINITION AXIOM
%token PROP TYPE FUN FORALL LET IN
%token FUN_ARROW IMPL_ARROW LOLLI_ARROW
%token COMMA COLON PERIOD ASTER
%token LPAREN RPAREN
%token EQ_ASGN
%token EOF
%right IMPL_ARROW LOLLI_ARROW FUN FORALL
%start instruction
%type <Instruction.instruction> instruction
%type <Term.term_ast> term
%type <Term.term_ast list> termlist
%type <Term.term_ast> term_single
%type <Term.term_ast> term_atom

%%

instruction:
  | EOF { EndOfInstruction }
  | DEFINITION IDENT COLON term EQ_ASGN term PERIOD {
      DefinitionInstruction ($2,$4,$6) }
  | AXIOM IDENT COLON term PERIOD {
      AxiomInstruction ($2,$4) }
term:
  | FUN IDENT COLON term FUN_ARROW term { TermAstFun ($2,$4,$6,false) }
  | FUN ASTER IDENT COLON term FUN_ARROW term { TermAstFun ($3,$5,$7,true) }
  | LET IDENT COLON term EQ_ASGN term IN term {
      TermAstLetIn ($2,$4,$6,$8,false) }
  | LET ASTER IDENT COLON term EQ_ASGN term IN term {
      TermAstLetIn ($3,$5,$7,$9,true) }
  | FORALL IDENT COLON term COMMA term { TermAstForall ($2,$4,$6,false) }
  | term IMPL_ARROW term { TermAstForall ("_",$1,$3,false) }
  | term LOLLI_ARROW term { TermAstForall ("_",$1,$3,true) }
  | term_single { $1 }
term_single:
  | termlist {
      List.fold_left (fun h t -> TermAstApply (h,t))
        (List.hd $1)
        (List.tl $1) }
termlist:
  | term_atom { [$1] }
  | term_atom termlist { $1::$2 }
term_atom:
  | IDENT { TermAstVarRef $1 }
  | TYPE LPAREN INT RPAREN { TermAstSort $3 }
  | PROP { TermAstSort 0 }
  | LPAREN term RPAREN { $2 }
;
