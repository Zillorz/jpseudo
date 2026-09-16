let inp = "
if a:
  print(2)
else if b:
  print(3)
else:
  print(4)
end if
";;

let toks = Lexer.parse inp;;
print_endline(Lexer.string_of_tok_list_debug toks);;
let ast = Ast.parse toks;;

let rec show_ast_list al = match al with
    | h::t ->
        let () = print_endline(Ast.show_ast h) in
        show_ast_list t
    | [] -> ();;

show_ast_list ast;;
