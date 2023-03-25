module Generation where

import GHC.Float (int2Float)
import Nets
import Utils

perceptron :: Phi -> Int -> [Int] -> Int -> Net
perceptron phi inCount hiddenCounts outCount =
  Net neurons inNeurons outNeurons
  where
    neurons = [Input | x <- [1 .. inCount]] ++ genLayers (hiddenCounts ++ [outCount]) inCount (inCount - 1)
      where
        genLayers :: [Int] -> Int -> Int -> [Neuron]
        genLayers [] _ _ = []
        genLayers (c : cs) prevCount currIndex =
          [McCullochPitts phi [(i, pseudoRand (int2Float currIndex + int2Float j + int2Float i)) | i <- [currIndex - (prevCount - 1) .. currIndex]] | j <- [1 .. c]]
            ++ genLayers cs c (currIndex + c)
    inNeurons = [0 .. inCount - 1]
    outNeurons = [outStartIndex .. totalNeuronsCount]
      where
        outStartIndex = inCount + sum hiddenCounts - 1
        totalNeuronsCount = outStartIndex + (outCount - 1)