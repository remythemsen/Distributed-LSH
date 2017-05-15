package messages

import actors.{DataSetFac, DisaParserFac, HashFunctionFactory}
import measures.Distance

case class InitRepetition[A](file:String, n:Int, parserFac:DisaParserFac[A], dataSetFac:DataSetFac[A], internalReps:Int, hashFunction:HashFunctionFactory[A], probeGenerator:String, maxCands:Int, functions:Int, dimensions:Int, simMeasure:Distance[A], seed:Long)
