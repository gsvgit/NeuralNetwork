module NeuralNetwork.Linear

open System
open FSharpx.Collections
open NeuralNetwork.LinearAlgebra

type ILayer<'Input, 'Output> =
    abstract Forward : input: 'Input -> 'Output
  
//type Linear<'t when 't:(static member (*) : ^t * ^t -> ^t) and 't:(static member (+) : ^t * ^t -> ^t)>
type Linear(weights:Matrix<float>, bias:array<float>) =
    interface ILayer<array<float>, array<float>> with
       member this.Forward input =
            vXm input weights (*) (+)
            |> Array.map2 (+) bias       
        
    new (inputSize:uint, outSize:uint) =
        let rand = Random()
        let weights = Matrix.init inputSize outSize (fun _ _ -> rand.NextDouble())
        let bias = Array.init (int outSize) (fun _ -> rand.NextDouble())
        Linear(weights, bias)
               
    override this.ToString() =
        sprintf $"Weights: \n %A{weights}"
        + "\n"
        + sprintf $"Bias: \n %A{bias}"
        
type ActivationLayer(activation: float -> float, name) =
    interface ILayer<array<float>, array<float>> with
        member this.Forward input =
            Array.map activation input            
    override this.ToString() =
        sprintf $"Activation function: %s{name}" + "\n"
                        

type Model () =
    let mutable forward :array<float> -> array<float> = id
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
        
module Linear =
    let linear (weights:Matrix<float>) (bias:array<float>) =
        let linear = Linear(weights, bias) :> ILayer<array<float>, array<float>>
        linear.Forward
        
    let d_linear (inputSize:uint) (outSize:uint) =
        let linear = Linear(inputSize, outSize) :> ILayer<array<float>, array<float>>
        linear.Forward
        
    let act_linear func name =
        let linear = ActivationLayer(func, name) :> ILayer<array<float>, array<float>>
        linear.Forward
        
    let (>~>) (model:Model) (layer:ILayer<_,_>) =
        model.Forward <- model.Forward >> (layer :> ILayer<array<float>, array<float>>).Forward
        model.AddLayer layer
        model
        
    let sigmoidFunc x =
        1.0/(1.0 + Math.Exp(-x))
