(* uses pratt parsing *)
(* https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html *)

(* Subset of Token for values/variables *)
type atom = 
  | IntValue of int
  | FloatValue of float
  | StringValue of string
  | Variable of string [@@deriving show];;

(* 
   An atom is the simplest object that than can be operated on.
   A unit is an operator applied to one other expression
   A cons is an operator applied to two expressions
   A list is a collection of expressions, more complex operators use this + unit for many expressions
*)
type expression =  
  | Atom of atom
  | Unit of Lexer.operator * expression
  | Cons of Lexer.operator * expression * expression
  | List of expression list [@@deriving show];;

(* Assigns order of operations *)
let prefix_bp operator = match operator with
| Lexer.Return -> 1
| Lexer.Mul -> 20
| Lexer.Sub -> 19
| _ -> failwith ("Operator " ^ Lexer.show_operator operator ^ " does not support prefix");;

let postfix_bp operator = match operator with
| Lexer.Inclusive -> Some 8
| _ -> None;;

let infix_bp operator = match operator with
| Lexer.Eq -> Some (2, 3)
| Lexer.Comma -> Some(4, 5)
| Lexer.To | Lexer.Downto -> Some(6, 7)
| Lexer.Or | Lexer.Xor -> Some(9, 10)
| Lexer.And -> Some(11, 12)
| Lexer.Deq | Lexer.Le | Lexer.Lt | Lexer.Gt | Lexer.Ge -> Some(13, 14)
| Lexer.Add | Lexer.Sub -> Some (15, 16)
| Lexer.Mul | Lexer.Div | Lexer.IntDiv | Lexer.Mod -> Some (17, 18)
| Lexer.Dot -> Some (21, 22)
| _ -> None;;

(* utility functions *)
let ensure_seperator:
  Lexer.seperator -> Lexer.token list -> (Lexer.token -> string) -> string -> Lexer.token list =
    fun ensure toks reason_wrong reason_missing ->

  match toks with
  | (Seperator ensure)::toks -> toks
  | [] -> failwith reason_missing
  | tok::toks -> failwith (reason_wrong tok);;

let rec extract_atom: Lexer.token list -> expression * Lexer.token list = fun list ->
  match list with
  | (Lexer.ConstInt const)::toks -> (Atom (IntValue const), toks)
  | (Lexer.ConstFloat const)::toks -> (Atom (FloatValue const), toks)
  | (Lexer.ConstStr const)::toks -> (Atom (StringValue const), toks)
  | (Lexer.Ident ident)::toks -> (Atom (Variable ident), toks)
  | other_tok::_ -> failwith("Cannot extract atom: " ^ (Lexer.string_of_token_debug other_tok))
  | _ -> failwith("Cannot extract atom from nothing")

let rec flatten_comma_list: expression -> expression list = fun comma_list_exp ->
  match comma_list_exp with
  | Cons (Lexer.Comma, comma_list, element) ->
      (* Maybe check if e2 is a comma list and flatten? opt for now *)
      element :: (flatten_comma_list comma_list)
  | element -> [element]

let rec parse_expr toks min_bp =
  match toks with
  (* If the tokens start with an operator, it must be a prefix operator *)
  | (Lexer.Operator op)::toks ->
      let (expr, toks) = parse_expr toks (prefix_bp op) in
      let lhs = Unit (op, expr) in
      parse_partial_expr lhs toks min_bp

  (* Parenthesis support *)
  | (Seperator Lexer.OpenParen)::toks -> (
      let (expr, toks) = parse_next_expression toks in
      let toks = ensure_seperator Lexer.ClosedParen toks
        (fun t -> "Expected ) found ^ " ^ Lexer.string_of_token_debug t)
        "Parenthesis not matched" in
 
      parse_partial_expr expr toks min_bp
  )

  (* Bracket list support *)
  | (Seperator Lexer.OpenBracket)::(Seperator Lexer.ClosedBracket)::toks -> (
      parse_partial_expr (List []) toks min_bp
  )

  | (Seperator Lexer.OpenBracket)::t -> (
      let (list, toks) = parse_next_expression t in
      let toks = ensure_seperator Lexer.ClosedBracket toks
        (fun t -> "(lst) Expected ] found " ^ Lexer.string_of_token_debug t)
        "(lst) Bracket not matched" in
      let list = (List (List.rev (flatten_comma_list list))) in

      parse_partial_expr list toks min_bp
  )

  (* In the basic case, extract the first atom and start the loop/upper match *)
  | _ ->
      let (atom, toks) = extract_atom toks in
      parse_partial_expr atom toks min_bp
and parse_partial_expr lhs toks min_bp =

  match toks with
  (* Our token list must start with an operator *)
  | (Lexer.Operator op)::toks -> (
    match postfix_bp op with
    | Some pf_bp ->
      if pf_bp < min_bp then
        (lhs, toks)
      else
        parse_partial_expr (Unit (op, lhs)) toks min_bp
    | None ->

    match infix_bp op with
    | Some (l_bp, r_bp) ->
        if l_bp < min_bp then
          (lhs, toks)
        else
          let (rhs, list) = parse_expr toks r_bp in
          parse_partial_expr (Cons (op, lhs, rhs)) toks min_bp
    | None -> (lhs, toks)
  )

  (* Special case for indexing *)
  | (Seperator Lexer.OpenBracket)::toks -> (
        let (index, toks) = parse_next_expression toks in
        let toks = ensure_seperator Lexer.ClosedBracket toks
          (fun t -> "(idx) Expected ] found " ^ Lexer.string_of_token_debug t)
          "(idx) Bracket not matched" in

        parse_partial_expr (Cons (Lexer.Index, lhs, index)) toks min_bp
  )

  (* Special case for function calls *)
  | (Seperator Lexer.OpenParen)::(Seperator Lexer.ClosedParen)::toks -> (
      parse_partial_expr (List []) toks min_bp
  )

  | (Seperator Lexer.OpenParen)::toks -> (
      let (args, toks) = parse_next_expression toks in
      let toks = ensure_seperator Lexer.ClosedParen toks
        (fun t -> "(call) Expected ) found " ^ Lexer.string_of_token_debug t)
        "(call) Parenthesis not matched" in

      let args = (List (List.rev (flatten_comma_list args))) in

      parse_partial_expr (Unit (Lexer.Call, args)) toks min_bp
  )

  (* If we didn't find an operator, we just return *)
  | _ -> (lhs, toks) 
and parse_next_expression toks =
  parse_expr toks 0
;;

let parse_expression list = match parse_next_expression list with | (a, b) -> a;;
