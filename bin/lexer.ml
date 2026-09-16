type keyword = 
  | Function
  | For
  | While
  | If
  | Else
  | End [@@deriving show];;

type seperator =
  | Colon
  | Semicolon
  | OpenParen
  | ClosedParen
  | OpenBracket
  | ClosedBracket
  | Newline [@@deriving show];;

type operator = 
  (* these operators exists but are parsed weirdly *)
  | Index
  | Comma
  | Call
  | Return

  (* operators for iterators *)
  | Downto
  | To
  | Inclusive

  | Eq
  | Deq (* == *)
  | Lt
  | Le
  | Gt
  | Ge
  | Add
  | Sub
  | Mul
  | Div
  | IntDiv (* // *)
  | Mod
  (* a.b *)
  | Dot
  (* Boolean operators *)
  | And
  | Or
  | Xor [@@deriving show];;

type token = 
  | Ident of string
  | Keyword of keyword
  | Seperator of seperator
  | Operator of operator
  | ConstInt of int
  | ConstFloat of float
  | ConstStr of string
  | Nop;;

let token_of_ident ident = Ident ident
let token_of_keyword kw = Keyword kw
let token_of_seperator sep = Seperator sep
let token_of_operator op = Operator op

let token_of_const_numeral numstr = match numstr with
(* Because ocaml's int_of_string is so good, we don't need to implement these cases *)
(* Replace if we wan't more custom parsing later *)
(* | hex when String.starts_with ~prefix:"0x" hex -> ConstInt (int_of_string hex) *)
(* | binary when String.starts_with ~prefix:"0b" binary -> ConstInt (int_of_string binary) *)
| float when String.contains float '.' -> ConstFloat (float_of_string float)
| regular -> ConstInt (int_of_string regular);;

let seperators = [
  (";", Semicolon);
  (":", Colon);
  ("(", OpenParen);
  (")", ClosedParen);
  ("[", OpenBracket);
  ("]", ClosedBracket);
  ("\n", Newline);
];;

let operators = [
  (">=", Ge);
  ("<=", Le);
  ("==", Deq);
  ("<", Lt);
  (">", Gt);
  ("=", Eq);
  ("+", Add);
  ("-", Sub);
  ("*", Mul);
  ("/", Div);
  ("//", IntDiv);
  ("%", Mod);
  ("and", And);
  ("or", Or);
  ("^", Xor);
  (".", Dot);
  ("inclusive", Inclusive);
  ("to", To);
  ("downto", To);
  (",", Comma);
  ("return", Return)
];;

let keywords = [
 ("for", For);
 ("while", While);
 ("function", Function);
 ("if", If);
 ("else", Else);
 ("end", End);
];;

(* Hopefully this is equal to ^[a-zA-Z][a-zA-Z0-9_]* because I only know normal regex lmao *)
let ident_regex =
  let open Re in
  seq [
    bos;
    alt [rg 'A' 'Z'; rg 'a' 'z'; char '_'];
    rep (alt [rg 'A' 'Z'; rg 'a' 'z'; rg '0' '9'; char '_'])
  ] |> compile;;

(* ^(0x[0-9A-F_]+)|(0b[01_]+)|(-?[0-9_]+\.?[0-9_]* ) *)
(* without the groups, allows for underscores, hex, and binary *)
let numerical_regex =
  let open Re in
  seq [
    bos;
    alt[
      seq[char '0'; char 'x'; rep1(alt[rg '0' '9'; rg 'A' 'F'; char '_'])];
      seq[char '0'; char 'b'; rep1(alt[char '0'; char '1'])];
      seq[opt(char '-'); rep1(alt[rg '0' '9'; char '_']); opt(char '.'); rep(alt[rg '0' '9'; char '_'])]
    ]
  ] |> compile

(* Our two general purpose map functions *)
let rec parse_mapped string map = match map with
| [] -> None
| (pfx, tok)::t -> 
    if String.starts_with ~prefix:pfx string then 
      let remaining = String.drop_first (String.length pfx) string in
      Some(tok, remaining) 
    else parse_mapped string t;;

let parse_rgx string rgx = match Re.exec_opt rgx string with
| Some g -> let v = Re.Group.get g 0 in
              let remaining = String.drop_first (String.length v) string in
              Some (v, remaining)
| None -> None;;

(* Go from most to least specific *)
(* Really ugly :( *)
let rec parse string = match String.drop_first_while (fun c -> c == ' ') string with 
| str when Option.is_some(parse_mapped str operators) ->
    let (tok, rem) = Option.get(parse_mapped str operators) in 
      token_of_operator tok :: parse(rem)      

| str when Option.is_some(parse_mapped str seperators) ->
    let (tok, rem) = Option.get(parse_mapped str seperators) in 
      token_of_seperator tok :: parse(rem)

| str when Option.is_some(parse_mapped str keywords) ->
    let (tok, rem) = Option.get(parse_mapped str keywords) in 
      token_of_keyword tok :: parse(rem)

| str when Option.is_some(parse_rgx str numerical_regex) ->
    let (tok, rem) = Option.get(parse_rgx str numerical_regex) in 
      token_of_const_numeral tok :: parse(rem)

| str when Option.is_some(parse_rgx str ident_regex) ->
    let (tok, rem) = Option.get(parse_rgx str ident_regex) in 
      token_of_ident tok :: parse(rem)
| _ -> [];;

let rec parse_function_args tokens =
  match tokens with
  | (Ident arg_name)::(Operator Comma)::toks ->
      let (args, rtoks) = parse_function_args toks in
      (arg_name :: args, rtoks)
  | (Ident arg_name)::(Seperator ClosedParen)::toks ->
      ([arg_name], toks)
  | (Seperator ClosedParen)::toks ->
      ([], toks)
  | _ -> failwith("Invalid function args");;

let string_of_token_debug t = match t with
| Ident iden -> "Ident(" ^ iden ^ ")"
| Operator op -> "Operator(" ^ show_operator op ^ ")"
| Seperator sep -> "Seperator(" ^ show_seperator sep ^ ")"
| Keyword kw -> "Keyword(" ^ show_keyword kw ^ ")"
| ConstInt ci -> "Const(" ^ string_of_int ci ^ ")"
| ConstFloat cf -> "Const(" ^ string_of_float cf ^ ")"
| ConstStr cs -> "Const( ^ cs ^ )"
| Nop -> "Nop";;

let rec string_of_tok_list_debug l = match l with
| [] -> ""
| h::t -> string_of_token_debug h ^ ", " ^ string_of_tok_list_debug t;;
