module Training where

import Nets
import Utils

type TrainingData = (NetInput, NetOutput)

type TrainingSet = [TrainingData]

backpropagationCycle :: Net -> TrainingData -> Net
backpropagationCycle net@(Net neurons inIndicies outIndicies) = undefined

mse :: Net -> TrainingData -> Float
mse net (input, expected) = (sum . map (** 2) $ zipWith (-) expected (netOut net input)) / 2