module NeuralNetwork.Flatten

open NeuralNetwork.Common
open NeuralNetwork.LinearAlgebra

type Flatten<'t>() =
    interface ILayer<Matrix<'t>, Matrix<'t>> with
        member this.Forward input =
            let result = Matrix(1u, input.Columns * input.Rows)
            //!!!
            result
