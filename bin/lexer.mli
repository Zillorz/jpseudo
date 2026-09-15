type keyword = 
  | For
  | If
  | Inclusive
  | End
  | Then
  | Return [@@deriving show];;

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
  | Dot

  | Eq
  | Deq (* == *)
  | Le
  | Leq
  | Ge
  | Geq
  | Add
  | Sub
  | Mul
  | Div
  | Mod
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

val string_of_token_debug : token -> string;;
val string_of_tok_list_debug : token list -> string;;
