package messages

import measures.Distance
import multiprobing.ProbeKeyGenerator

case class InitRepetition(filePath:String, n:Int, internalRepetitions:Int, hashFunction:String, functions:Int, dimensions:Int, simMeasure:Distance, seed:Long)
