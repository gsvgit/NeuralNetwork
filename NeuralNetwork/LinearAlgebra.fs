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
        
let mXm (leftMatrix:Matrix<'a>) (rightMatrix:Matrix<'b>) (opMult:'a -> 'b -> 'c) (opAdd: 'c -> 'c -> 'c) =
    let result = Matrix(leftMatrix.Rows, rightMatrix.Columns)
    if leftMatrix.Columns <> rightMatrix.Rows
    then failwithf $"Can not multiply vector of size {leftMatrix.Columns} and matrix with {rightMatrix.Rows} rows"
    for i in 0u..leftMatrix.Rows - 1u do
        for j in 0u..rightMatrix.Columns - 1u do
            for k in 0u..rightMatrix.Rows - 1u do 
                result[int i, int j] <- opAdd result[int i, int j] (opMult leftMatrix[int i, int k] rightMatrix[int k, int j])
    result
    
module Matrix = 
    let map2 f m1 m2 = m1
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
        
    let conv input kernel = input