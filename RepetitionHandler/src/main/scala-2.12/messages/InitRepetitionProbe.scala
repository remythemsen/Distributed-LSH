package messages

import measures.Distance
import multiprobing.ProbeKeyGenerator

case class InitRepetitionProbe(filePath:String, n:Int, internalRepetitions:Int, hashFunction:String, probeGenerator:ProbeKeyGenerator, maxCands:Int, functions:Int, dimensions:Int, simMeasure:Distance, seed:Long)
