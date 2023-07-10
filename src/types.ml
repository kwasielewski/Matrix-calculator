type myType =
  | Unit
  | Integer
  | Floating
  | Matrix of int * int
  | Toeplitz of int * int
  | TU of int * int
  | Real_invertible of int * int
  | Orthogonal of int * int
  | Unimodular of int * int
  | Permutation of int * int

type myAbstractType =
  | AMatrix
  | AToeplitz
  | ATU
  | AReal_invertible
  | AOrthogonal
  | AUnimodular
  | APermutation
