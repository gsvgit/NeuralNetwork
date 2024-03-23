module NeuralNetwork.Math

open System

let sigmoid x =
    1.0/(1.0 + Math.Exp(-x))
        
(*
let softmax

*)