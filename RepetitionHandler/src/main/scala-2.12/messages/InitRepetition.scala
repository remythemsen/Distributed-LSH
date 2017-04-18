package messages

import actors.{DisaParserFac, HashFunctionFactory}
import measures.Distance

case class InitRepetition[A](filePath:String, n:Int, parserFac:DisaParserFac[A], internalReps:Int, hashFunction:HashFunctionFactory[A], probeGenerator:String, maxCands:Int, functions:Int, dimensions:Int, simMeasure:Distance[A], seed:Long)
