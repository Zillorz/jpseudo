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

val parse : Lexer.token list -> ast list
