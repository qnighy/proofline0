{
  open Parser
  let keyword_table = Hashtbl.create 101
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "fun", FUN;
      "forall", FORALL;
      "Type", TYPE ]
}
rule token =
  parse [' ' '\t' '\n'] { token lexbuf }
      | "(*" { in_comment lexbuf; token lexbuf }
      | ['0'-'9']+ as num { INT (int_of_string num) }
      | (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*) as id {
          try
            Hashtbl.find keyword_table id
          with Not_found ->
            IDENT id }
      | "=>" { FUN_ARROW }
      | "->" { IMPL_ARROW }
      | "," { COMMA }
      | ":" { COLON }
      | "-@" { LOLLI_ARROW }
      | "(" { LPAREN }
      | ")" { RPAREN }
      | eof { EOT }
and in_comment =
  parse "(*" { in_comment lexbuf; in_comment lexbuf }
      | "*)" { () }
      | _ { in_comment lexbuf }
