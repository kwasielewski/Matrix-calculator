%start <Ast.toplevel> operations

%{
open Types
open Ast
let mk_node data = {
    tag = fresh_node_tag ();
    data = data
}

let string_to_atp x =
    (match x with
    | "permutation" -> APermutation
    | "toeplitz" -> AToeplitz
    | "tu" -> ATU 
    | "realinv" -> AReal_invertible
    | "unimodular" -> AUnimodular
    | "orthogonal" -> AOrthogonal
    | _ -> AMatrix
    )


%}
%token <string> IDENTIFIER
%token <string> CONSTRUCTOR
%token <int64> INT 
%token <float> FLOAT
%token LBRACKET
%token RBRACKET
%token SEMICOLON
%token PLUS MULT MINUS
%token LPAREN
%token RPAREN
%token ANS
%token PRINT
%token EOF

%left PLUS MINUS
%left MULT
(*use --infer*)
%type <numeric> nums
%type <expression> operation
%%

operations:
    | EOF
    {Top([])}
    | es = nonempty_list(operation) EOF
    {Top(es)}

operation:
    | CONSTRUCTOR LPAREN LBRACKET coeffs = separated_nonempty_list(SEMICOLON, nonempty_list(nums)) RBRACKET RPAREN
    {mk_node(EXPR_Matrix ((string_to_atp $1),coeffs))}
    | LBRACKET coeffs = separated_nonempty_list(SEMICOLON, nonempty_list(nums)) RBRACKET
    {mk_node(EXPR_Matrix (AMatrix,coeffs))}
    | l=operation MULT r=operation
    {mk_node(EXPR_Binop{op=OP_Mult;lhs=l;rhs=r})}
    | l=operation MINUS r=operation
    {mk_node(EXPR_Binop{op=OP_Minus;lhs=l;rhs=r})}
    | l=operation PLUS r=operation
    {mk_node(EXPR_Binop{op=OP_Plus;lhs=l;rhs=r})}
    | PRINT 
    {mk_node(EXPR_Print)}
    | ANS
    {mk_node(EXPR_Ans)}

nums:
    | FLOAT 
    {Floatn $1}
    | INT 
    {Intn $1}
