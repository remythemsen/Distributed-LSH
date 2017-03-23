package messages

import hashfunctions.HashFunction
import measures.Distance

case class InitRepetition(filePath:String, hf:() => HashFunction, dimensions:Int, simMeasure:Distance)
