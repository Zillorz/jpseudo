<<<<<<< Updated upstream
let inp = "
if a:
  print(2)
else if b:
  print(3)
else:
  print(4)
end if
";;
=======
let inp ="
  function binaryserach(A,t):
    L = 0
    R = len(A)-1
    while L <= R:
      C = floor((L+R)/2)
      if A[C] == t:
        return C
      elseif A[C] < t:
        L = C + 1
      else:
        R = C - 1
      end if
    end while
    return FAIL
  end function
"
>>>>>>> Stashed changes

let toks = Lexer.parse inp;;

print_endline (Lexer.string_of_tok_list_debug toks);;

let ast = Ast.parse toks

let rec show_ast_list al =
  match al with
  | h :: t ->
      let () = print_endline (Ast.show_ast h) in
      show_ast_list t
  | [] -> ()
;;

show_ast_list ast
