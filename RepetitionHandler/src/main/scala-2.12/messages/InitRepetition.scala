package messages

import actors.HashFunctionFactory
import hashfunctions.HashFunction
import io.Parser.DisaParser
import measures.Distance

case class InitRepetition[A](filePath:String, n:Int, dataParser:DisaParser[A], internalRepetitions:Int, hashFunction:HashFunctionFactory[A], probeGenerator:String, maxCands:Int, functions:Int, dimensions:Int, simMeasure:Distance[A], seed:Long)
