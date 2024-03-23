module main

open NeuralNetwork
open NeuralNetwork.Activation
open NeuralNetwork.Flatten
open NeuralNetwork.Linear
open NeuralNetwork.Convolution

let m2 =
    let m = 
        Model()
        >~> Linear(3u, 2u)
        >~> sigmoid
        >~> Linear(2u, 2u)
        >~> sigmoid
        >~> Linear(2u, 1u)
        >~> sigmoid
        
    printfn $"Model:\n%A{m.ToString()}"
    m.Forward

(*printfn $"%A{m2 [|1.0; 2.0; 3.0|]}"
printfn $"%A{m2 [|1.0; 2.0; 3.0|]}"
printfn $"%A{m2 [|2.0; 3.0; 5.0|]}"
printfn $"%A{m2 [|0.0; 0.0; 0.0|]}"
*)
let m3 =
    let m = 
        Model()
        >~> Convolution(3u)
        >~> sigmoid
        >~> Convolution(3u)
        >~> sigmoid
        >~> Flatten()
        >~> Linear(2u, 1u)
        >~> sigmoid
        
    printfn $"Model:\n%A{m.ToString()}"
    m.Forward

