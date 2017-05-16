package io

import java.util

import scala.collection.mutable

object Parser {

  abstract class DisaParser[A](iterator: Iterator[String], dimensions:Int) extends Iterator[(Int, A)] {

    override def hasNext: Boolean = iterator.hasNext

    override def next: (Int, A)
  }

  case class DisaParserRaw(iterator: Iterator[String], dimensions:Int) extends DisaParser[Array[Float]](iterator, dimensions) {
    /*
    * Expected format for file is:
    * id
    * comp   comp  comp   comp ... comp(i) .. comp(dimensions)
    *
    * ####################################      (x49) 1234
    * 1.3143 2.031 4.3102 -12.212
    */
    override def next: (Int, Array[Float]) = {
      (iterator.next.substring(49).toInt, iterator.next.split(" ").map(x => x.toFloat))
    }
  }

  case class DisaParserNumeric(iterator: Iterator[String], dimensions:Int) extends DisaParser[Array[Float]](iterator, dimensions) {
    /*
    * Expected format for file is:
    * id   comp   comp  comp   comp ... comp(i) .. comp(dimensions)
    * 1234 1.3143 2.031 4.3102 -12.212
    */
    override def next: (Int, Array[Float]) = {
      val line = iterator.next.split(" ")
      (line.head.toInt, line.tail.map(x => x.toFloat))
    }

      /*val resultArray:Array[Float] = new Array(dimensions)

      //val tuple = split(iterator.next, dimensions + 1) // number of components of vector + id
      val line = iterator.next
      var id = 0
      var sc = true
      var j,fc = 0
      while(sc) {
        if(line.charAt(j) == ' ') {
          sc = false
          fc = j+1
          id = line.substring(0, j).toInt
        }
        j+=1
      }
      val vector = line.substring(fc)
      var i,ws,c = 0
      while(i < vector.length) {
        if(vector.charAt(i) == ' ') {
          // we just passed over a 'component', it's between ws and i-1
          resultArray(c) = vector.substring(ws, i).toFloat

          // setting next 'component' start
          ws = i+1
          c+=1
        }
        i+=1
      }
      (id, resultArray)
    }*/
  }
  case class DisaParserBinary(iterator: Iterator[String], dimensions:Int) extends DisaParser[util.BitSet](iterator, dimensions) {
    /*
    * Expected format for file is:
    * id  binaryString
    * 1234 1010100010010100101
    */
    override def next: (Int, util.BitSet) = {

      val line:Array[String] = iterator.next.split(" ")
      val bitSet:util.BitSet = new util.BitSet()
      val l = line(1).split("")
      var i = 0
      while(i < l.length) {
        if(l(i) == "1") {
          bitSet.set(i)
        }
        i+=1
      }

      (line.head.toInt, bitSet) // convert bitstring to bitset
    }
  }
}
