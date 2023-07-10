open Ast
open Types

let type_to_string_suffix tp =
  match Typechecker.concrete_to_abstract tp with
  | AMatrix -> "Mat"
  | AOrthogonal -> "Orth"
  | APermutation -> "Perm"
  | AReal_invertible -> "Realinv"
  | ATU -> "TU"
  | AToeplitz -> "Toep"
  | AUnimodular -> "Uni"

module CGen = struct
  (*not working due to problems with pointers*)
  let includes = [ "matlib.h"; "stdio.h" ]

  (*add filename later*)
  let list_to_literal ls =
    "{"
    ^ String.concat ", "
        (List.map
           (fun x ->
             match x with
             | Intn i -> Int64.to_string i
             | Floatn f -> Float.to_string f)
           ls)
    ^ "}"

  let listlist_toLiteral lss =
    "{" ^ String.concat ", " (List.map list_to_literal lss) ^ "}"

  let gen typed_forest =
    let tpes = types_of_typed_expr (List.hd typed_forest) in
    let _ = print_endline "Types passed" in
    (*now support for only one operation*)
    let gen_includes inc =
      String.concat "\n" (List.map (fun x -> "#include<" ^ x ^ ">") inc) ^ "\n"
    in

    let dimstr tp =
      let n, m = Typechecker.dimensions tp in
      Int.to_string n ^ ", " ^ Int.to_string m
    in

    let rec op_to_code op =
      match op.data with
      | EXPR_Matrix (_, ess) -> listlist_toLiteral ess
      | EXPR_Binop { op = OP_Plus; lhs = l; rhs = r } ->
          "add(" ^ op_to_code l ^ ", " ^ op_to_code r ^ ")"
      | _ -> failwith "not yet"
    in

    let op_to_code_with_cost texpr =
      let find_min texpr =
        List.fold_left
          (fun x y -> if snd x < snd y then x else y)
          (Integer, Int.max_int)
          (types_of_typed_expr texpr)
      in
      let tp, cost = find_min texpr in
      let rec oprec texpr =
        match texpr with
        | TEXPR_Matrix (_, ess, tp) -> listlist_toLiteral ess
        (*simple types are just written*)
        | TEXPR_Binop { op = OP_Mult; lhs = l; rhs = r; tp } ->
            let _ = print_endline "BINOP" in
            let mintp = find_min texpr in
            let lopt, ropt =
              Typechecker.find_optimal_pair (types_of_typed_expr l)
                (types_of_typed_expr r) 0 (fst mintp) (snd mintp)
            in
            let _ = print_endline "After BINOP" in
            "mult"
            ^ type_to_string_suffix (fst lopt)
            ^ type_to_string_suffix (fst ropt)
            ^ "(" ^ oprec l ^ ", "
            ^ dimstr (fst lopt)
            ^ ", " ^ oprec r ^ ", "
            ^ dimstr (fst ropt)
            ^ ")"
        | TEXPR_Binop { op = OP_Plus; lhs = l; rhs = r; tp } ->
            let mintp = find_min texpr in
            let lopt, ropt =
              Typechecker.find_optimal_pair (types_of_typed_expr l)
                (types_of_typed_expr r) 1 (fst mintp) (snd mintp)
            in
            "add"
            ^ type_to_string_suffix (fst lopt)
            ^ type_to_string_suffix (fst ropt)
            ^ "(" ^ oprec l ^ ", "
            ^ dimstr (fst lopt)
            ^ ", " ^ oprec r ^ ", "
            ^ dimstr (fst ropt)
            ^ ")"
        | TEXPR_Binop { op = OP_Minus; lhs = l; rhs = r; tp } ->
            let mintp = find_min texpr in
            let lopt, ropt =
              Typechecker.find_optimal_pair (types_of_typed_expr l)
                (types_of_typed_expr r) 1 (fst mintp) (snd mintp)
            in
            "sub"
            ^ type_to_string_suffix (fst lopt)
            ^ type_to_string_suffix (fst ropt)
            ^ "(" ^ oprec l ^ ", "
            ^ dimstr (fst lopt)
            ^ ", " ^ oprec r ^ ", "
            ^ dimstr (fst ropt)
            ^ ")"
        | _ -> failwith "not yet"
      in

      oprec texpr
    in

    let gen_body op meat =
      "int main(){\n"
      ^ (match fst op with
        (*change int to correct type*)
        | Matrix (n, m) | Permutation (n, m) ->
            "\tint res1[][" ^ Int.to_string m ^ "] = "
        | _ -> failwith "unknown type in gen_body")
      ^ meat ^ ";\n}"
    in

    let _ = print_endline "type " in

    let oper = List.hd tpes in
    let _ = print_endline ("found type " ^ type_to_string_suffix (fst oper)) in
    let first_inst = List.hd typed_forest in
    gen_includes includes ^ gen_body oper (op_to_code_with_cost first_inst)
end

module JGen = struct
  let includes = [ "julmat.jl" ]

  (*add filename later*)
  let list_to_literal ls =
    ""
    ^ String.concat " "
        (List.map
           (fun x ->
             match x with
             | Intn i -> Int64.to_string i
             | Floatn f -> Float.to_string f)
           ls)
    ^ ""

  let type_to_string_suffix tp =
    match Typechecker.concrete_to_abstract tp with
    | AMatrix -> "Mat"
    | AOrthogonal -> "Orth"
    | APermutation -> "Perm"
    | AReal_invertible -> "Realinv"
    | ATU -> "TU"
    | AToeplitz -> "Toep"
    | AUnimodular -> "Uni"

  let listlist_toLiteral lss =
    "[" ^ String.concat "; " (List.map list_to_literal lss) ^ "]"

  let gen typed_forest =
    (*tpes is old version*)
    let tpes = types_of_typed_expr (List.hd typed_forest) in

    let gen_includes inc =
      String.concat "\n" (List.map (fun x -> "include(\"" ^ x ^ "\")") inc)
      ^ "\n"
    in

    let dimstr tp =
      let n, m = Typechecker.dimensions tp in
      Int.to_string n ^ ", " ^ Int.to_string m
    in

    let rec op_to_code op =
      match op.data with
      | EXPR_Matrix (_, ess) -> listlist_toLiteral ess
      | EXPR_Binop { op = OP_Plus; lhs = l; rhs = r } ->
          "add(" ^ op_to_code l ^ ", " ^ op_to_code r ^ ")"
      | _ -> failwith "not yet"
    in

    let op_to_code_with_cost texpr id =
      let find_min texpr =
        List.fold_left
          (fun x y -> if snd x < snd y then x else y)
          (Integer, Int.max_int)
          (types_of_typed_expr texpr)
      in
      let tp, cost = find_min texpr in
      let rec oprec texpr =
        match texpr with
        | TEXPR_Matrix (_, ess, tp) -> listlist_toLiteral ess
        (*simple types are just written*)
        | TEXPR_Binop { op = OP_Mult; lhs = l; rhs = r; tp } ->
            let mintp = find_min texpr in
            let lopt, ropt =
              Typechecker.find_optimal_pair (types_of_typed_expr l)
                (types_of_typed_expr r) 0 (fst mintp) (snd mintp)
            in

            "mult"
            ^ type_to_string_suffix (fst lopt)
            ^ type_to_string_suffix (fst ropt)
            ^ "(" ^ oprec l ^ ", "
            ^ dimstr (fst lopt)
            ^ ", " ^ oprec r ^ ", "
            ^ dimstr (fst ropt)
            ^ ")"
        | TEXPR_Binop { op = OP_Plus; lhs = l; rhs = r; tp } ->
            let mintp = find_min texpr in
            let lopt, ropt =
              Typechecker.find_optimal_pair (types_of_typed_expr l)
                (types_of_typed_expr r) 1 (fst mintp) (snd mintp)
            in
            "add"
            ^ type_to_string_suffix (fst lopt)
            ^ type_to_string_suffix (fst ropt)
            ^ "(" ^ oprec l ^ ", "
            ^ dimstr (fst lopt)
            ^ ", " ^ oprec r ^ ", "
            ^ dimstr (fst ropt)
            ^ ")"
        | TEXPR_Binop { op = OP_Minus; lhs = l; rhs = r; tp } ->
            let mintp = find_min texpr in
            let lopt, ropt =
              Typechecker.find_optimal_pair (types_of_typed_expr l)
                (types_of_typed_expr r) 1 (fst mintp) (snd mintp)
            in
            "sub"
            ^ type_to_string_suffix (fst lopt)
            ^ type_to_string_suffix (fst ropt)
            ^ "(" ^ oprec l ^ ", "
            ^ dimstr (fst lopt)
            ^ ", " ^ oprec r ^ ", "
            ^ dimstr (fst ropt)
            ^ ")"
        | TEXPR_Ans _ -> "res" ^ Int.to_string (id - 1)
        | _ -> failwith "not yet"
      in

      oprec texpr
    in

    let gen_body op meat =
      "function main()\n"
      ^ (match fst op with
        (*change int to correct type*)
        | Matrix (n, m) | Permutation (n, m) -> "\tres1 = "
        | _ -> failwith "unknown type in gen_body")
      ^ meat ^ "\nend\nmain()\n"
    in

    let gen_body2 typed_forest =
      (*multiple instructions*)
      let rec gen_rec typed_forest acc id =
        match typed_forest with
        | [] -> acc
        | TEXPR_Print _ :: xs ->
            gen_rec xs
              (acc ^ "\tprintln(res" ^ Int.to_string (id - 1) ^ ")\n")
              id
        | x :: xs ->
            gen_rec xs
              (acc ^ "\tres" ^ Int.to_string id ^ "= "
             ^ op_to_code_with_cost x id ^ "\n")
              (id + 1)
      in

      "function main()\n" ^ gen_rec typed_forest "" 0 ^ "end\nmain()\n"
    in

    let oper = List.hd tpes in
    let first_inst = List.hd typed_forest in
    gen_includes includes ^ gen_body2 typed_forest
end
