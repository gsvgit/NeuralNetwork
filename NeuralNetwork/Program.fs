open NeuralNetwork.Linear
open NeuralNetwork.Linear.Linear

let m =
    d_linear 3u 2u
    >> d_linear 2u 2u
    >> d_linear 2u 1u
    >> act_linear sigmoidFunc "Sigmoid"
    
printfn $"%A{m [|1.0; 2.0; 3.0|]}"
printfn $"%A{m [|1.0; 2.0; 3.0|]}"
printfn $"%A{m [|2.0; 3.0; 5.0|]}"
printfn $"%A{m [|0.0; 0.0; 0.0|]}"

let m2 =
    let m = 
        Model()
        >~> Linear(3u, 2u)
        >~> Linear(2u, 2u)
        >~> Linear(2u, 1u)
        >~> ActivationLayer(sigmoidFunc, "Sigmoid")
    printfn $"Model:\n%A{m.ToString()}"
    m.Forward

printfn $"%A{m2 [|1.0; 2.0; 3.0|]}"
printfn $"%A{m2 [|1.0; 2.0; 3.0|]}"
printfn $"%A{m2 [|2.0; 3.0; 5.0|]}"
printfn $"%A{m2 [|0.0; 0.0; 0.0|]}"
