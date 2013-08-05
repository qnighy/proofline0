%{
  open Term
%}
%token <string> IDENT
%token <int> INT
%token TYPE
%token FUN
%token FORALL
%token FUN_ARROW
%token COMMA
%token COLON
%token IMPL_ARROW
%token LOLLI_ARROW
%token LPAREN RPAREN
%token EOT
%right IMPL_ARROW LOLLI_ARROW FUN FORALL
%start main
%type <Term.term_ast> main
%type <Term.term_ast> term
%type <Term.term_ast list> termlist
%type <Term.term_ast> term_complex

%%

main:
  | term EOT { $1 }
term:
  | termlist {
      List.fold_left (fun h t -> TermAstApply (h,t))
        (List.hd $1)
        (List.tl $1) }
termlist:
  | term_complex { [$1] }
  | term_complex termlist { $1::$2 }
term_complex:
  | IDENT { TermAstVarRef $1 }
  | LPAREN term RPAREN { $2 }
  | TYPE LPAREN INT RPAREN { TermAstSort $3 }
  | FUN IDENT COLON term FUN_ARROW term { TermAstFun ($2,$4,$6) }
  | FORALL IDENT COLON term COMMA term { TermAstForall ($2,$4,$6) }
  | term IMPL_ARROW term { TermAstForall ("_",$1,$3) }
  | term LOLLI_ARROW term { TermAstLolli ($1,$3) }
;
