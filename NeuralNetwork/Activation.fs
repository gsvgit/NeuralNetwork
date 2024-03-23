module NeuralNetwork.Activation

open NeuralNetwork.Common
open NeuralNetwork.LinearAlgebra

type Activation(activation: float -> float, name) =
    interface ILayer<Matrix<float>, Matrix<float>> with
        member this.Forward input =
            Matrix.map activation input            
    override this.ToString() =
        sprintf $"Activation function: %s{name}" + "\n"
        
let sigmoid = Activation(Math.sigmoid,"sigmoid")
        


