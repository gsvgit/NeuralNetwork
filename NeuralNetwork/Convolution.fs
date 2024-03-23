module NeuralNetwork.Convolution

open System
open NeuralNetwork.Common
open NeuralNetwork.LinearAlgebra

type Convolution(kernel: Matrix<float>) =
    interface ILayer<Matrix<float>, Matrix<float>> with
       member this.Forward input =
            Matrix.conv input kernel
            
    new (kernelSize: uint) =
        let rand = Random()
        let weights = Matrix.init kernelSize kernelSize (fun _ _ -> rand.NextDouble())
        Convolution(weights)
    