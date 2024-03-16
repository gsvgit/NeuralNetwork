module NeuralNetwork.LinearAlgebra

type Matrix<'t>(rows:uint, columns:uint) =
    let store = Array.init (int rows) (fun _ -> Array.zeroCreate<'t> (int columns))
    member this.Item
        with get (i, j) = store[i][j]
        and set (i, j) v = store[i][j] <- v
    member this.Rows = rows
    member this.Columns = columns
    override this.ToString() =
        store
        |> Array.map (fun a ->
            sprintf $"%A{a}")
        |> String.concat "\n"
        
let vXm (vector:array<'a>) (matrix:Matrix<'b>) (opMult:'a -> 'b -> 'c) (opAdd: 'c -> 'c -> 'c) =
    let result = Array.zeroCreate (int matrix.Columns)
    if vector.Length <> (int matrix.Rows)
    then failwithf $"Can not multiply vector of size {vector.Length} and matrix with {matrix.Rows} rows"
    for i in 0u..matrix.Columns - 1u do
        for j in 0u..matrix.Rows - 1u do
            result[int i] <- opAdd result[int i] (opMult vector[int j] matrix[int j, int i])
    result
    
module Matrix = 
    let map (f: 'a -> 'b) (matrix:Matrix<'a>) =
        let res = Matrix<'b>(matrix.Rows, matrix.Columns)
        for i in 0u..matrix.Rows - 1u do
            for j in 0u..matrix.Columns - 1u do
                res[int i, int j] <- f matrix[int i, int j]
        res
    let init rows columns f =
        let res = Matrix<'b>(rows, columns)
        for i in 0u..rows - 1u do
            for j in 0u..columns - 1u do
                res[int i, int j] <- f i j
        res