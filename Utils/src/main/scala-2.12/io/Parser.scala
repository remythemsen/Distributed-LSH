package io

import java.io.{File, InputStream}

import io.Parser.DisaParser

import scala.io.Source

object Parser {
  type Descriptor = (Int, Array[Float])

  case class DisaParser(iterator: Iterator[String], dimensions:Int) extends Iterator[Descriptor] {
    /*
    * Expected format for file is:
    * id   comp   comp  comp   comp ... comp(i) .. comp(dimensions)
    * 1234 1.3143 2.031 4.3102 -12.212
    */

    override def hasNext: Boolean = iterator.hasNext

    override def next: Descriptor = {
      val tuple = split(iterator.next, dimensions + 1) // number of components of vector + id
      (tuple.head.toInt, tuple.tail)
    }
  }

  /*
  * Useful mem mapped iterator
  */
  // Will not work if file is > 2GB
  def memMappedIterator(file:File): Iterator[String] = {
    import java.io.FileInputStream
    import java.nio.ByteOrder.LITTLE_ENDIAN
    import java.nio.channels.FileChannel

    import scala.io.Source

    val channel = new FileInputStream(file).getChannel
    val buffer = channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size)
    buffer.order(LITTLE_ENDIAN)

    val stream = new InputStream {
      override def read() = if(buffer.hasRemaining) buffer.get else -1
    }

    Source.fromInputStream(stream).getLines()
  }

  /*
  * Efficient string split by whitespace, using indexOf
   */
  def split(s:String, l:Int) = {
    @annotation.tailrec
    def go(end:Int, pos:Int, arrI:Int, auxArr: Array[Float]) : Array[Float] = {
      if(end >= 0) {
        auxArr(arrI) = s.substring(pos, end).toFloat
        go(s.indexOf(' ', end+1), end+1 , arrI + 1, auxArr)
      } else auxArr
    }
    go(s.indexOf(' ', 0), 0, 0, new Array[Float](l))
  }
}
object Program extends App {
  DisaParser(Source.fromFile(new File("data/descriptors-1-million-reduced-128.data")).getLines, 128)
}
