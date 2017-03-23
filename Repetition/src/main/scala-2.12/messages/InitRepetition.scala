package messages

import hashfunctions.HashFunction
import measures.Distance

case class InitRepetition(filePath:String, hashFunction:String, functions:Int, dimensions:Int, simMeasure:Distance, seed:Long)
