open Ast
open Types

module Subtyping = Graph.Persistent.Digraph.ConcreteBidirectional (struct
  type t = myAbstractType

  let compare = compare
  let hash = Hashtbl.hash
  let equal a b = a = b
end)

let create_structure () =
  let hierarchy = Subtyping.add_vertex Subtyping.empty AMatrix in
  let hierarchy = Subtyping.add_vertex hierarchy AReal_invertible in
  let hierarchy = Subtyping.add_vertex hierarchy AOrthogonal in
  let hierarchy = Subtyping.add_vertex hierarchy AUnimodular in
  let hierarchy = Subtyping.add_vertex hierarchy APermutation in
  let hierarchy = Subtyping.add_vertex hierarchy AToeplitz in
  let hierarchy = Subtyping.add_vertex hierarchy ATU in
  hierarchy

let structure = create_structure ()

let create_type_hierarchy () =
  let hierarchy = structure in

  let hierarchy = Subtyping.add_edge hierarchy AReal_invertible AMatrix in
  let hierarchy = Subtyping.add_edge hierarchy AOrthogonal AReal_invertible in
  let hierarchy = Subtyping.add_edge hierarchy AUnimodular AReal_invertible in
  let hierarchy = Subtyping.add_edge hierarchy APermutation AOrthogonal in
  let hierarchy = Subtyping.add_edge hierarchy APermutation AUnimodular in
  let hierarchy = Subtyping.add_edge hierarchy AToeplitz AMatrix in
  let hierarchy = Subtyping.add_edge hierarchy ATU AUnimodular in
  hierarchy

let create_available_functions () =
  let addt = Hashtbl.create 13 in
  let multt = Hashtbl.create 13 in
  (*semirandom availability just for showing functionality*)
  Hashtbl.add addt (AMatrix, AMatrix) AMatrix;
  Hashtbl.add multt (AMatrix, AMatrix) AMatrix;
  Hashtbl.add multt (APermutation, APermutation) AMatrix;
  Hashtbl.add multt (AUnimodular, AOrthogonal) AMatrix;
  Hashtbl.add multt (AOrthogonal, AUnimodular) AMatrix;
  Hashtbl.add multt (AMatrix, AOrthogonal) AMatrix;

  (addt, multt)

let create_function_costs () =
  let addc = Hashtbl.create 13 in
  let multc = Hashtbl.create 13 in
  (*semirandom costs just for showing functionality*)
  Hashtbl.add addc (AMatrix, AMatrix) (fun x y z w -> 5 * x * y);
  Hashtbl.add multc (AMatrix, AMatrix) (fun x y z w -> 10 * x * y * w);
  Hashtbl.add multc (APermutation, APermutation) (fun x y z w -> 1 * x * y * w);
  Hashtbl.add multc (AUnimodular, AOrthogonal) (fun x y z w -> 3 * x * y * w);
  Hashtbl.add multc (AOrthogonal, AUnimodular) (fun x y z w -> 3 * x * y * w);
  Hashtbl.add multc (AMatrix, AOrthogonal) (fun x y z w -> 5 * x * y * w);
  (addc, multc)

let hierarchy = create_type_hierarchy ()
let adds, mults = create_available_functions ()
let addcost, multcost = create_function_costs ()

let abstract_to_concrete atp n m =
  match atp with
  | AMatrix -> Matrix (n, m)
  | AToeplitz -> Toeplitz (n, m)
  | ATU -> TU (n, m)
  | AReal_invertible -> Real_invertible (n, m)
  | AOrthogonal -> Orthogonal (n, m)
  | AUnimodular -> Unimodular (n, m)
  | APermutation -> Permutation (n, m)

let dimensions ctp =
  match ctp with
  | Matrix (n, m) -> (n, m)
  | Toeplitz (n, m) -> (n, m)
  | TU (n, m) -> (n, m)
  | Real_invertible (n, m) -> (n, m)
  | Orthogonal (n, m) -> (n, m)
  | Unimodular (n, m) -> (n, m)
  | Permutation (n, m) -> (n, m)
  | Unit | Integer | Floating -> (0, 0)

let concrete_to_abstract ctp =
  match ctp with
  | Matrix _ -> AMatrix
  | Toeplitz _ -> AToeplitz
  | TU _ -> ATU
  | Real_invertible _ -> AReal_invertible
  | Orthogonal _ -> AOrthogonal
  | Unimodular _ -> AUnimodular
  | Permutation _ -> APermutation
  | Unit | Integer | Floating ->
      failwith "no abstract type for Integer and Floating"

let type_to_string_suffix tp =
  match concrete_to_abstract tp with
  | AMatrix -> "Mat"
  | AOrthogonal -> "Orth"
  | APermutation -> "Perm"
  | AReal_invertible -> "Realinv"
  | ATU -> "TU"
  | AToeplitz -> "Toep"
  | AUnimodular -> "Uni"

let cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)

let rec new_types ltp rtp op =
  let func = if op = 1 then adds else mults in
  let types = [] in
  let types =
    match Hashtbl.find_opt func (ltp, rtp) with
    | Some v -> [ v ]
    | None ->
        let cart =
          cartesian
            (Subtyping.succ hierarchy ltp)
            (Subtyping.succ hierarchy rtp)
          @ List.map (fun x -> (ltp, x)) (Subtyping.succ hierarchy rtp)
          @ List.map (fun x -> (x, rtp)) (Subtyping.succ hierarchy ltp)
        in
        List.fold_left (fun acc x -> new_types (fst x) (snd x) op @ acc) [] cart
  in
  types

let rec new_types_with_cost ltpc rtpc op =
  let ltp, lc = ltpc in
  let rtp, rc = rtpc in
  let altp = concrete_to_abstract ltp in
  let artp = concrete_to_abstract rtp in
  let n1, m1 = dimensions ltp in
  let n2, m2 = dimensions rtp in
  let n3, m3 = if op = 1 then (n1, m1) else (n1, m2) in
  let funav = if op = 1 then adds else mults in
  let func = if op = 1 then addcost else multcost in
  let types = [] in
  let types =
    match Hashtbl.find_opt funav (altp, artp) with
    | Some v ->
        [
          ( abstract_to_concrete v n3 m3,
            lc + rc + (Hashtbl.find func (altp, artp)) n1 m1 n2 m2 );
        ]
    | None ->
        let cart =
          cartesian
            (match altp with
            | _ -> Subtyping.succ hierarchy altp)
            (match artp with
            | _ -> Subtyping.succ hierarchy artp)
          @ List.map (fun x -> (altp, x)) (Subtyping.succ hierarchy artp)
          @ List.map (fun x -> (x, artp)) (Subtyping.succ hierarchy altp)
        in
        List.fold_left
          (fun acc x ->
            new_types_with_cost
              (abstract_to_concrete (fst x) n1 m1, lc)
              (abstract_to_concrete (snd x) n2 m2, rc)
              op
            @ acc)
          [] cart
  in
  types

let rec new_types_with_cost_list ltps rtps op =
  let cart = cartesian ltps rtps in
  List.fold_left
    (fun acc x -> new_types_with_cost (fst x) (snd x) op @ acc)
    [] cart

let rec search_types ltpc rtpc op endtp cost =
  let ltp, lc = ltpc in
  let rtp, rc = rtpc in
  let altp = concrete_to_abstract ltp in
  let artp = concrete_to_abstract rtp in
  let n1, m1 = dimensions ltp in
  let n2, m2 = dimensions rtp in
  let n3, m3 = if op = 1 then (n1, m1) else (n1, m2) in
  let funav = if op = 1 then adds else mults in
  let func = if op = 1 then addcost else multcost in
  let types = [] in
  let types =
    match Hashtbl.find_opt funav (altp, artp) with
    | Some v ->
        [
          ( ltpc,
            rtpc,
            abstract_to_concrete v n3 m3 = endtp
            && lc + rc + (Hashtbl.find func (altp, artp)) n1 m1 n2 m2 = cost );
        ]

    | None ->
        let cart =
          cartesian
            (match altp with
            | _ -> Subtyping.succ hierarchy altp)
            (match artp with
            | _ -> Subtyping.succ hierarchy artp)
          @ List.map (fun x -> (altp, x)) (Subtyping.succ hierarchy artp)
          @ List.map (fun x -> (x, artp)) (Subtyping.succ hierarchy altp)
        in

        List.fold_left
          (fun acc x ->
            search_types
              (abstract_to_concrete (fst x) n1 m1, lc)
              (abstract_to_concrete (snd x) n2 m2, rc)
              op endtp cost
            @ acc)
          [] cart
  in
  types

let rec find_optimal_pair ltps rtps op endtp cost =
  match
    let cart = cartesian ltps rtps in
    List.hd
      (List.filter
         (fun x -> match x with x, y, z -> z)
         (List.fold_left
            (fun acc x -> search_types (fst x) (snd x) op endtp cost @ acc)
            [] cart))

  with
  | x, y, z -> (x, y)

let rec new_types_list ltps rtps op =
  let cart = cartesian ltps rtps in
  List.fold_left (fun acc x -> new_types (fst x) (snd x) op @ acc) [] cart

let select_min te =
  match te with
  | TEXPR_Id (i, t) -> TEXPR_Id (i, [ List.hd t ])
  | TEXPR_Int (i, t) -> TEXPR_Int (i, [ List.hd t ])
  | TEXPR_Float (f, t) -> TEXPR_Float (f, [ List.hd t ])
  | TEXPR_Matrix (a, n, t) -> TEXPR_Matrix (a, n, [ List.hd t ])
  | TEXPR_Call { id; args; tp } -> TEXPR_Call { id; args; tp = [ List.hd tp ] }
  | TEXPR_Ans t -> TEXPR_Ans [ List.hd t ]
  | TEXPR_Print t -> TEXPR_Print [ List.hd t ]
  | TEXPR_Binop { op; lhs; rhs; tp } ->
      TEXPR_Binop { op; lhs; rhs; tp = [ List.hd tp ] }

let anstp_type a = match a with None -> None | Some v -> Some (select_min v)

let infer top =
  let rec infer_expr e anstp =
    match Ast.get_data e with
    | EXPR_Int i ->
        let res = TEXPR_Int (i, [ (Integer, 0) ]) in
        (res, Some res)
    | EXPR_Float f ->
        let res = TEXPR_Float (f, [ (Floating, 0) ]) in
        (res, Some res)
    | EXPR_Matrix (atp, m) ->
        let a = List.length m in
        if a = 0 then failwith "matrix size"
        else
          let b = List.length (List.hd m) in
          if b > 0 && List.for_all (fun el -> List.length el = b) (List.tl m)
          then
            let res =
              TEXPR_Matrix (atp, m, [ (abstract_to_concrete atp a b, 0) ])
            in
            (res, Some res)
          else failwith "matrix size"
    | EXPR_Binop { op = OP_Mult; lhs = l; rhs = r } ->
        (*currently no error handling*)
        let ltp = fst (infer_expr l anstp) in
        let rtp = fst (infer_expr r anstp) in
        let n1, m1 = dimensions (fst (List.hd (types_of_typed_expr ltp))) in
        let n2, m2 = dimensions (fst (List.hd (types_of_typed_expr rtp))) in
        if m1 <> n2 then failwith "multiplication dimensions doesnt match"
        else
          let res =
            TEXPR_Binop
              {
                op = OP_Mult;
                lhs = ltp;
                rhs = rtp;
                tp =
                  new_types_with_cost_list (types_of_typed_expr ltp)
                    (types_of_typed_expr rtp) 0;
              }
          in
          (res, Some res)
    | EXPR_Binop { op = OP_Minus; lhs = l; rhs = r } ->
        let ltp = fst (infer_expr l anstp) in
        let rtp = fst (infer_expr r anstp) in
        let n1, m1 = dimensions (fst (List.hd (types_of_typed_expr ltp))) in
        let n2, m2 = dimensions (fst (List.hd (types_of_typed_expr rtp))) in
        if n1 <> n2 || m1 <> m2 then failwith "dimensions doesnt match"
        else
          let res =
            TEXPR_Binop
              {
                op = OP_Minus;
                lhs = ltp;
                rhs = rtp;
                tp =
                  new_types_with_cost_list (types_of_typed_expr ltp)
                    (types_of_typed_expr rtp) 1;
              }
          in
          (res, Some res)
    | EXPR_Binop { op = OP_Plus; lhs = l; rhs = r } ->
        let ltp = fst (infer_expr l anstp) in
        let rtp = fst (infer_expr r anstp) in
        let n1, m1 = dimensions (fst (List.hd (types_of_typed_expr ltp))) in
        let n2, m2 = dimensions (fst (List.hd (types_of_typed_expr rtp))) in
        if n1 <> n2 || m1 <> m2 then failwith "dimensions doesnt match"
        else
          let res =
            TEXPR_Binop
              {
                op = OP_Plus;
                lhs = ltp;
                rhs = rtp;
                tp =
                  new_types_with_cost_list (types_of_typed_expr ltp)
                    (types_of_typed_expr rtp) 1;
              }
          in
          (res, Some res)
    | EXPR_Ans -> (
        match anstp with
        | None -> failwith "ans is not defined"
        | Some v -> (TEXPR_Ans (types_of_typed_expr v), anstp))
    | EXPR_Print -> (TEXPR_Print [ (Unit, 0) ], anstp)
    | _ -> failwith "not yet"
  in
  List.rev
    (fst
       (List.fold_left
          (fun acc e ->
            let res = infer_expr e (snd acc) in
            (fst res :: fst acc, anstp_type (snd res)))
          ([], None) (Ast.get_expr_list top)))
