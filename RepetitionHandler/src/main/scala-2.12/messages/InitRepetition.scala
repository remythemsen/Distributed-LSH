package messages

import hashfunctions.HashFunction
import measures.Distance

case class InitRepetition(filePath:String, n:Int, internalRepetitions:Int, hashFunction:String, functions:Int, dimensions:Int, simMeasure:Distance, seed:Long)
