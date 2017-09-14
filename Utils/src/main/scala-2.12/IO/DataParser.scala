package IO

case class DataParser(iterator: Iterator[String]) extends Iterator[(Int, Array[Double])] {
  /*
  * Expected format for data is:
  * id   comp   comp  comp   comp ... comp(i) .. comp(dimensions)
  * 1234 1.3143 2.031 4.3102 -12.212
  */
  override def next: (Int, Array[Double]) = {
    val line = iterator.next.split(" ")
    (line.head.toInt, line.tail.map(x => x.toDouble))
  }

  override def hasNext: Boolean = this.iterator.hasNext

}
