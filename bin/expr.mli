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

val condense: Lexer.token list -> expression;;
val extract_atom: Lexer.token list -> expression * Lexer.token list;;
