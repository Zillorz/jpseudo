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

val token_of_ident : string -> token;;
val token_of_keyword : keyword -> token;;
val token_of_seperator : seperator -> token;;
val token_of_operator : operator -> token;;
val token_of_const_numeral : string -> token;;

val parse : string -> token list;;
val parse_function_args: token list -> string list * token list;;

val string_of_token_debug : token -> string;;
val string_of_tok_list_debug : token list -> string;;
