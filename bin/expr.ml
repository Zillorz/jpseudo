(* Subset of Token for values/variables *)
type atom = 
  | IntValue of int
  | FloatValue of float
  | StringValue of string
  | Variable of string [@@deriving show];;

(* 
   An atom is the simplest object that than can be operated on.
   A unit is an expression applied to one other expression
   A cons is an expression applied to two expressions
*)
type expression =  
  | Atom of atom
  | Unit of Lexer.operator * expression
  | Cons of Lexer.operator * expression * expression [@@deriving show];;

(* Assigns order of operations *)
let prefix_bp o = match o with
| Lexer.Mul -> 14
| Lexer.Sub -> 13
| _ -> failwith("Operator " ^ Lexer.show_operator o ^ " does not support prefix");;

let postfix_bp o = match o with
| _ -> None;;

let infix_bp o = match o with
| Lexer.Eq -> Some (1, 2)
| Lexer.Or | Lexer.Xor -> Some(3, 4)
| Lexer.And -> Some(5, 6)
| Lexer.Deq | Lexer.Le | Lexer.Leq | Lexer.Ge | Lexer.Geq -> Some(7, 8)
| Lexer.Add | Lexer.Sub -> Some (9, 10)
| Lexer.Mul | Lexer.Div | Lexer.Mod -> Some (11, 12)
| Lexer.Dot -> Some (15, 16)
| _ -> None;;

(* uses pratt parsing *)
(* https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html *)
let rec extract_atom: Lexer.token list -> expression * Lexer.token list = fun l ->
  match l with
  | (Lexer.ConstInt ci)::t -> (Atom (IntValue ci), t)
  | (Lexer.ConstFloat cf)::t -> (Atom (FloatValue cf), t)
  | (Lexer.ConstStr cs)::t -> (Atom (StringValue cs), t)
  | (Lexer.Ident id)::t -> (Atom (Variable id), t)
  | h::t -> failwith("Unexpected Token " ^ (Lexer.string_of_token_debug h))
  | _ -> failwith("Cannot extract atom from empty tokens")
  

(* this function is magic, it works perfectly, but is super confusing *)
let rec cond: expression option -> Lexer.token list -> int -> expression * Lexer.token list = fun lhs list min_bp ->
  (* This function can be called two ways, either with some inital LHS expression or without*)
  match lhs with

  (* The inital lhs expression exists *)
  | Some lhs -> (
  match list with
  (* Our token list must start with an operator, or we quit *)
  | (Lexer.Operator op)::t -> (
    match postfix_bp(op), infix_bp(op) with
    | Some pf_bp, None -> (* postfix op *)
        if (pf_bp < min_bp) then
          (lhs, list)
        else
          let lhs = Unit (op, lhs) in
          (cond (Some lhs) t min_bp)

    | _, Some (l_bp, r_bp) -> (* infix op *)
        if l_bp < min_bp then
          (lhs, list)
        else
          let (expr, list) = cond None t r_bp in
          let lhs = Cons (op, lhs, expr) in
          (cond (Some lhs) list min_bp)

    | _, _ -> (lhs, list) (* neither *)
  )


  (* Special case for indexing *)
  | (Lexer.Seperator ob)::t when ob == Lexer.OpenBracket -> ( 
        let (expr, list) = cond None t 0 in
        
        match list with
        | (Lexer.Seperator s)::t when s == Lexer.ClosedBracket ->
            let lhs = Cons (Lexer.Index, lhs, expr) in
            cond (Some lhs) t min_bp
        | h::t -> failwith("Expected ] found " ^ Lexer.string_of_token_debug h)
        | [] -> failwith("Bracket not matched.")
  )

  (* If we didn't find an operator, we just return *)
  | _ -> (lhs, list) 
  )


  (* In this case, there is no LHS, so we start anew *)
  | None ->
  match list with
  (* If the list starts with an operator, it must be a prefix operator, as there is no LHS for infix*)
  | (Lexer.Operator op)::t ->
      let (expr, list) = cond None t (prefix_bp op) in
      let lhs = Unit (op, expr) in
      cond (Some lhs) list min_bp

  (* Parenthesis support *)
  | (Lexer.Seperator s)::t when s == Lexer.OpenParen -> ( 
      let (expr, list) = cond None t 0 in
      
      match list with
      | (Lexer.Seperator s)::t when s == Lexer.ClosedParen ->
          cond (Some expr) t min_bp
      | h::t -> failwith("Expected ), found " ^ Lexer.string_of_token_debug h)
      | _ -> failwith("Parenthesis not matched")
  )

  (* In the basic case, extract the first atom and start the loop/upper match *)
  | _ ->
      let (atom, list) = extract_atom list in
      cond (Some atom) list min_bp

let rec condense list = match cond None list 0 with | (a, b) -> a;;
