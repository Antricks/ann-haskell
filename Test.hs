module Test where

import Generation
import Nets
import Training

testNet1 :: Net
testNet1@(Net neurons1 inputs1 outputs1) = Net [Input, Input, Input, McCullochPitts (Sigmoid 1) [(0, 0.5), (1, 0.2), (2, 0.3)]] [0, 1, 2] [3]

testNet2@(Net neurons2 inputs2 outputs2) = perceptron Tanh 3 [2, 4, 2] 1