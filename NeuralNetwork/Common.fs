module NeuralNetwork.Common

type ILayer<'Input, 'Output> =
    abstract Forward : input: 'Input -> 'Output
