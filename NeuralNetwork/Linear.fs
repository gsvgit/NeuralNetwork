module NeuralNetwork.Linear

open FSharpx.Collections
open NeuralNetwork.LinearAlgebra
  
//type Linear<'t when 't:(static member (*) : ^t * ^t -> ^t) and 't:(static member (+) : ^t * ^t -> ^t)>
type Linear(activation, weights:Matrix<float>, bias:array<float>) =
    let forward input =
        vXm input weights (*) (+)
        |> Array.map2 (+) bias
        |> Array.map activation        
        
    new (inputSize:uint, outSize:uint, activation) =
        let rand = System.Random()
        let weights = Matrix.init inputSize outSize (fun _ _ -> rand.NextDouble())
        let bias = Array.init (int outSize) (fun _ -> rand.NextDouble())
        Linear(activation, weights, bias)
        
    member this.Forward input =
        forward input
        
    override this.ToString() =
        sprintf $"Weights: \n %A{weights}"
        + "\n"
        + sprintf $"Bias: \n %A{bias}"

type Model () =
    let mutable forward :array<float> -> array<float> = id
    let layers = ResizeArray<Linear>()
    member this.Forward
        with get () = forward
        and set v = forward <- v
    member this.AddLayer l = layers.Add l
    override this.ToString () =
        layers
        |> ResizeArray.mapi (fun i l ->
            sprintf $"Layer {i}: %A{l}")
        |> String.concat "\n"
        
module Linear =
    let linear (activation:float -> float) weights bias =
        let linear = Linear(activation, weights, bias)
        linear.Forward
        
    let d_linear inputSize outSize (activation:float -> float) =
        let linear = Linear(inputSize, outSize, activation)
        linear.Forward
        
    let (>~>) (model:Model) (layer:Linear) =
        model.Forward <- model.Forward >> layer.Forward
        model.AddLayer layer
        model
