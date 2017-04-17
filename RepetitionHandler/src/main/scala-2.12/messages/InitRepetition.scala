package messages

import actors.HashFunctionFactory
import measures.Distance

case class InitRepetition[A](filePath:String, n:Int, internalRepetitions:Int, hashFunction:HashFunctionFactory[A], probeGenerator:String, maxCands:Int, functions:Int, dimensions:Int, simMeasure:Distance[A], seed:Long)
