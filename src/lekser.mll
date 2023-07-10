{
  open Lexing
  open Parser 
  type token = Parser.token
  let create_hashtable size init =
    let tbl = Hashtbl.create size in
      List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keyword_table = 
    create_hashtable 2[
      ("ans", ANS);
      ("print", PRINT)
    ]
  let constructor_table = 
    create_hashtable 13[
      ("toeplitz", CONSTRUCTOR("toeplitz"));
      ("tu", CONSTRUCTOR("tu"));
      ("realinv", CONSTRUCTOR("realinv"));
      ("unimodular", CONSTRUCTOR("unimodular"));
      ("orthogonal", CONSTRUCTOR("orthogonal"));
      ("permutation", CONSTRUCTOR("permutation"))
    ]
  
}

let identifier = ['a'-'z' 'A'-'Z']+
let digits = ['0'-'9']+
let whitespace = [' ' '\t' '\r']+
let integer = ['-' '+']? digits
let floating = ['-' '+']? digits '.' digits
(*float is used only when needed*)

rule token = parse
  | ['\n']
  {new_line lexbuf; token lexbuf}
  | eof
  {EOF}
  | whitespace
  {token lexbuf}
  | '(' {LPAREN}
  | ')' {RPAREN}
  | '[' {LBRACKET}
  | ']' {RBRACKET}
  | ';' {SEMICOLON}
  | '*' {MULT}
  | '+' {PLUS}
  | '-' {MINUS}
  | floating as f {
    match Float.of_string_opt f with
    | Some v -> FLOAT v
    | None -> failwith "error parsing float"
  }
    | integer as i {
      match Int64.of_string_opt i with 
      | Some v -> INT v
      | None -> failwith "error parsing integer"
  }
  | identifier as id 
    {try 
      let tok = Hashtbl.find keyword_table id in 
      tok
      with Not_found ->
      (try
        let tok = Hashtbl.find constructor_table id in 
          tok 
        with Not_found ->
        IDENTIFIER(id))
    }
  
and line_comment = parse
  | '\n'
  {new_line lexbuf; token lexbuf}
  | eof
  { EOF}
  | _
  {line_comment lexbuf}
