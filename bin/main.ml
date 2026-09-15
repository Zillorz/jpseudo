let inp = "(1 + x[y]  -a.y) * (3 - 4)";;
let toks = Lexer.parse inp;;
print_endline(Lexer.string_of_tok_list_debug toks);;
let ast = Expr.condense toks;;
print_endline (Expr.show_expression ast);;
