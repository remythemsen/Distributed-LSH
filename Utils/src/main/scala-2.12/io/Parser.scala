package io

import scala.collection.mutable

object Parser {

  abstract class DisaParser[A](iterator: Iterator[String], dimensions:Int) extends Iterator[(Int, A)] {

    override def hasNext: Boolean = iterator.hasNext

    override def next: (Int, A)
  }

  case class DisaParserNumeric(iterator: Iterator[String], dimensions:Int) extends DisaParser[Array[Float]](iterator, dimensions) {
    /*
    * Expected format for file is:
    * id   comp   comp  comp   comp ... comp(i) .. comp(dimensions)
    * 1234 1.3143 2.031 4.3102 -12.212
    */
    override def next: (Int, Array[Float]) = {
      //val tuple = split(iterator.next, dimensions + 1) // number of components of vector + id
      val line = iterator.next.split(" ")
      (line.head.toInt, line.tail.map(x => x.toFloat))
    }
  }
  case class DisaParserBinary(iterator: Iterator[String], dimensions:Int) extends DisaParser[mutable.BitSet](iterator, dimensions) {
    /*
    * Expected format for file is:
    * id  binaryString
    * 1234 1010100010010100101
    */
    override def next: (Int, mutable.BitSet) = {

      val line:Array[String] = iterator.next.split(" ")
      val bitSet:mutable.BitSet = new mutable.BitSet()
      val l = line(1).split("")
      var i = 0
      while(i < l.length) {
        if(l(i) == "1") {
          bitSet(i)
        }
        i+=1
      }

      (line.head.toInt, bitSet) // convert bitstring to bitset
    }
  }
}
