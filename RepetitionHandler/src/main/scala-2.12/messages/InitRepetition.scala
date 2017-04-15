package messages

import measures.Distance
import multiprobing.ProbeScheme

case class InitRepetition(filePath:String, n:Int, internalRepetitions:Int, hashFunction:String, probeGenerator:String, maxCands:Int, functions:Int, dimensions:Int, simMeasure:Distance, seed:Long)
