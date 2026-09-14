let inp = "
x = 5
for x in -y:
  b(x)
end for
";;

print_endline (string_of_tok_list_debug (parse_tok inp))
