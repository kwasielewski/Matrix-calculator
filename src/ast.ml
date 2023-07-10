type node_tag = NodeTag of int

let fresh_node_tag =
  let next = ref 0 in
  fun () ->
    let r = !next in
    next := r + 1;
    NodeTag r

type 'a node = { tag : node_tag; data : 'a }
type identifier = Identifier of string
type op = OP_Plus | OP_Minus | OP_Mult
type numeric = Intn of int64 | Floatn of float

type expression = expression_data node

and expression_data =
  | EXPR_Id of identifier
  | EXPR_Int of int
  | EXPR_Float of float
  | EXPR_Matrix of Types.myAbstractType * numeric list list
  | EXPR_Call of { id : identifier; args : expression list }
  | EXPR_Ans
  | EXPR_Print
  | EXPR_Binop of { op : op; lhs : expression; rhs : expression }

type typed_expression = typed_expression_data node

and typed_expression_data =
  | TEXPR_Id of identifier * (Types.myType * int) list
  | TEXPR_Int of int * (Types.myType * int) list
  | TEXPR_Float of float * (Types.myType * int) list
  | TEXPR_Matrix of
      Types.myAbstractType * numeric list list * (Types.myType * int) list
  | TEXPR_Call of {
      id : identifier;
      args : typed_expression_data list;
      tp : (Types.myType * int) list;
    }
  | TEXPR_Ans of (Types.myType * int) list
  | TEXPR_Print of (Types.myType * int) list
  | TEXPR_Binop of {
      op : op;
      lhs : typed_expression_data;
      rhs : typed_expression_data;
      tp : (Types.myType * int) list;
    }

let types_of_typed_expr e =
  match e with
  | TEXPR_Id (_, t) -> t
  | TEXPR_Int (_, t) -> t
  | TEXPR_Float (_, t) -> t
  | TEXPR_Matrix (_, _, t) -> t
  | TEXPR_Call { id; args; tp } -> tp
  | TEXPR_Ans t -> t
  | TEXPR_Print t -> t
  | TEXPR_Binop { op; lhs; rhs; tp } -> tp

let get_data e = e.data

type toplevel = Top of expression list

let get_expr_list t = match t with Top es -> es
