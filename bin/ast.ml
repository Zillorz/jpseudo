type ast =
  | Expression of Expr.expression
  | Block of ast list
  (* An expression = to Cons(Eq, ident, iterator) and the loop body *)
  | For of Expr.expression * ast
  (* An expression of bool and the loop body *)
  | While of Expr.expression * ast
  (* An expression of bool, the if body, and the else(if) body *)
  | If of Expr.expression * ast * ast option
  (* args and body **)
  | Function of string list * ast
[@@deriving show]

(* utility functions *)
let ensure_seperator :
    Lexer.seperator -> Lexer.token list -> string -> Lexer.token list =
 fun ensure toks reason ->
  match toks with Seperator ensure :: toks -> toks | _ -> failwith reason

let ensure_end_of :
    Lexer.keyword -> Lexer.token list -> string -> Lexer.token list =
 fun ensure toks reason ->
  match toks with
  | Keyword Lexer.End :: Keyword ensure :: toks -> toks
  | _ -> failwith reason

let add_ast : ast -> ast list * Lexer.token list -> ast list * Lexer.token list
    =
 fun as_unit list ->
  let ast, tokens = list in
  (as_unit :: ast, tokens)

let rec parse_ast : Lexer.token list -> ast list * Lexer.token list =
 fun toks ->
  match toks with
  (* removes \n and ; at front*)
  | Seperator Lexer.Newline :: toks | Seperator Lexer.Semicolon :: toks ->
      parse_ast toks
  (* return for processing, the code expecting the 'end' 'else' will call parse_ast again *)
  | Keyword Lexer.End :: _ | Keyword Lexer.Else :: _ -> ([], toks)
  | Keyword Lexer.For :: toks ->
      let iteration, toks = Expr.parse_next_expression toks in
      (* extract Y in for Y:*)
      let toks =
        ensure_seperator Lexer.Colon toks
          "expected for loop iteration statement to end with colon"
      in
      (* ensure colon *)
      let body, toks = parse_ast toks in
      (* parse the body as an ast *)
      let toks =
        ensure_end_of Lexer.For toks "expected for loop to end with 'end for"
      in
      (* ensure 'end for'' *)

      add_ast (For (iteration, Block body)) (parse_ast toks)
  (* add the for loop to the ast, parse the rest *)
  (* All other constructs are based on for loop code (function, while loop, if) *)
  | Keyword Lexer.While :: toks ->
      let condition, toks = Expr.parse_next_expression toks in
      let toks =
        ensure_seperator Lexer.Colon toks
          "expected while loop condition to end with colon"
      in
      let body, toks = parse_ast toks in
      let toks =
        ensure_end_of Lexer.While toks
          "expected while loop to end with 'end while"
      in

      add_ast (While (condition, Block body)) (parse_ast toks)
  (* If is special, and complicated *)
  | Keyword Lexer.If :: toks -> (
      let condition, toks = Expr.parse_next_expression toks in
      let toks =
        ensure_seperator Lexer.Colon toks
          "expected if condition to end with colon"
      in
      let body, toks = parse_ast toks in

      match toks with
      | Keyword Lexer.End :: Keyword Lexer.If :: toks ->
          add_ast (If (condition, Block body, None)) (parse_ast toks)
      | Keyword Lexer.Else :: Keyword Lexer.If :: toks -> (
          let remaining_ast, remaining_toks =
            parse_ast (Keyword Lexer.If :: toks)
          in

          match remaining_ast with
          | else_if_ast :: remaining_ast ->
              let if_ast = If (condition, Block body, Some else_if_ast) in
              add_ast if_ast (remaining_ast, remaining_toks)
          | _ -> failwith "unreachable")
      | Keyword Lexer.Else :: toks ->
          let toks =
            ensure_seperator Lexer.Colon toks "else must be followed by colon"
          in
          let else_body, toks = parse_ast toks in
          let toks =
            ensure_end_of Lexer.If toks "else must be closed with end if"
          in

          add_ast
            (If (condition, Block body, Some (Block else_body)))
            (parse_ast toks)
      | _ -> failwith "if statement must end with 'end if', 'else if' or 'else'"
      )
  | Keyword Lexer.Function :: toks ->
      let name, toks =
        match toks with
        | Lexer.Ident name :: toks -> (name, toks)
        | _ -> failwith "function keyword must be followed by function name"
      in
      let toks =
        ensure_seperator Lexer.OpenParen toks
          "function name must be followed by ("
      in
      let args, toks = Lexer.parse_function_args toks in
      let toks =
        ensure_seperator Lexer.Colon toks
          "function definition must be followed by :"
      in
      let body, toks = parse_ast toks in
      let toks =
        ensure_end_of Lexer.Function toks
          "function must be closed with 'end function'"
      in

      add_ast (Function (args, Block body)) (parse_ast toks)
  | [] -> ([], [])
  (* if all else fails, parse a expression *)
  | toks -> (
      let expression, toks = Expr.parse_next_expression toks in

      let () = print_endline (Expr.show_expression expression) in

      match toks with
      | Seperator Lexer.Semicolon :: toks | Seperator Lexer.Newline :: toks ->
          add_ast (Expression expression) (parse_ast toks)
      | [] -> ([ Expression expression ], toks)
      | tok :: _ ->
          failwith
            ("expression must be followed by newline, semicolon, or EOF, found "
            ^ Lexer.string_of_token_debug tok))

let parse : Lexer.token list -> ast list =
 fun toks -> match parse_ast toks with a, b -> a
