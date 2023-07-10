# Matrix-calculator
Matrix calculator with type inference and subtyping. 

The main idea is to utilize knowledge about matrix types to select optimal algorithms for given calculation.
This calculator produces code in Julia that can be run to obtain final result.
Adding new matrix types and operations (currently only addition and multiplication) is described below.

## Requirements
* Ocamllex
* Menhir
* Ocamlgraph

## How to build and run

```
make all
make install
./matrix
```

## Example

The following code
```
permutation([1 0; 0 1]) * orthogonal([0 1; 0 1])
ans + [1 2; 3 4]
print
```
will produce this result
```julia=
include("julmat.jl")
function main()
	res0= multUniOrth([1 0; 0 1], 2, 2, [0 1; 0 1], 2, 2)
	res1= addMatMat(res0, 2, 2, [1 2; 3 4], 2, 2)
	println(res1)
end
main()
```

## How to add additional types and operations
To add a type:
* add type to adt in types.ml
* add constructor syntax to lekser.mll
* add type to type hierarchy in typechecker.ml

To add an operation:
* add operation to available operations in typechecker.ml
* add its cost in typechecker.ml
* add its implementation in julmat.jl

## Tips

You may consider using [rlwrap](https://github.com/hanslub42/rlwrap) to allow the editing of keyboard input.


