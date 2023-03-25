module Nets where

import Numeric
import Utils

type NeuronIndex = Int

type InValue = Float

type NetValue = Float

type OutValue = Float

type Weight = Float

type NetInput = [InValue]

type NetOutput = [OutValue]

type ActivFun = (NetValue -> OutValue)

type ActivDeriv = (NetValue -> Float)

data Phi = Tanh | Sigmoid Float | Threshhold deriving (Show, Eq)

data Net = Net [Neuron] [NeuronIndex] [NeuronIndex] deriving (Show, Eq) -- neurons, in neurons, out neurons

data Neuron
  = McCullochPitts Phi [(NeuronIndex, Weight)] -- incoming connections @ [(index of incoming neuron, weight of neurons output)]
  | Input
  | Bias

instance Eq Neuron where
  McCullochPitts ap aconns == McCullochPitts bp bconns = ap == bp && aconns == bconns
  Input == Input = True
  Bias == Bias = True
  a == b = False

instance Show Neuron where
  show :: Neuron -> String
  show (McCullochPitts phi conns) = "MCP " ++ show conns
  show Input = "Input"
  show Bias = "Bias"

--

threshhold :: ActivFun
threshhold x
  | x < 0 = 0
  | otherwise = 1

sigmoid :: Float -> ActivFun
sigmoid a x = 1 / (1 + exp (-a * x))

sigmoid' :: Float -> ActivDeriv
sigmoid' a x = a * exp (-a * x) / ((1 + exp (-a * x)) ** 2)

tanh' :: ActivDeriv
tanh' x = 1 / (cosh x ** 2)

phi :: Phi -> ActivFun
phi Tanh = tanh
phi (Sigmoid a) = sigmoid a
phi Threshhold = threshhold

phi' :: Phi -> ActivDeriv
phi' Tanh = tanh'
phi' (Sigmoid a) = sigmoid' a
phi' Threshhold = undefined

--

neuronNetInput :: Net -> NetInput -> NeuronIndex -> NetValue
neuronNetInput net@(Net netNeurons inNeurons outNeurons) inputs neurInd =
  sum $ map (\(j, w) -> w * neuronOut net inputs j) conns
  where
    neur@(McCullochPitts phi conns) = netNeurons !! neurInd

neuronOut :: Net -> NetInput -> NeuronIndex -> OutValue
neuronOut net@(Net netNeurons inNeurons outNeurons) inputs neurInd =
  case neur of
    McCullochPitts p conns -> phi p $ neuronNetInput net inputs neurInd
    Bias -> 1
    Input -> inputs !! indexOf inNeurons neurInd
  where
    neur = netNeurons !! neurInd

netOut :: Net -> NetInput -> NetOutput
netOut net@(Net _ _ outNeurons) inputs = [neuronOut net inputs i | i <- outNeurons]