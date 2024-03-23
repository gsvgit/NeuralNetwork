module NeuralNetwork.Linear

open System
open FSharpx.Collections
open NeuralNetwork.Activation
open NeuralNetwork.Common
open NeuralNetwork.LinearAlgebra
  
//type Linear<'t when 't:(static member (*) : ^t * ^t -> ^t) and 't:(static member (+) : ^t * ^t -> ^t)>
type Linear(weights:Matrix<float>, bias:Matrix<float>) =
    interface ILayer<Matrix<float>, Matrix<float>> with
       member this.Forward input =
            mXm input weights (*) (+)
            |> Matrix.map2 (+) bias       
        
    new (inputSize:uint, outSize:uint) =
        let rand = Random()
        let weights = Matrix.init inputSize outSize (fun _ _ -> rand.NextDouble())
        let bias = Matrix.init 1u outSize (fun _ _ -> rand.NextDouble())
        Linear(weights, bias)
               
    override this.ToString() =
        sprintf $"Weights: \n %A{weights}"
        + "\n"
        + sprintf $"Bias: \n %A{bias}"
        
type Model () =
    let mutable forward: Matrix<float> -> Matrix<float> = id
    let layers = ResizeArray<ILayer<_,_>>()
    member this.Forward
        with get () = forward
        and set v = forward <- v
    member this.AddLayer l = layers.Add l
    override this.ToString () =
        layers
        |> ResizeArray.mapi (fun i l ->
            sprintf $"Layer {i}: \n %A{l}")
        |> String.concat "\n"
    
let (>~>) (model:Model) (layer:ILayer<_,_>) =
    model.Forward <- model.Forward >> layer.Forward
    model.AddLayer layer
    model
    
    