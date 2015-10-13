let parse_file fname = 
  let org_in_chnl = open_in fname in
  let lexbuf = Lexing.from_channel org_in_chnl in
       print_string "Parsing ...\n" ;
       print_string fname ;
       print_string "\n" ;
       let prog = MOOL_parser.input (MOOL_lexer.token fname) lexbuf in
         close_in org_in_chnl ;
         prog
  