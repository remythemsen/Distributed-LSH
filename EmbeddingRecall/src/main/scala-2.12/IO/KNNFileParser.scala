package IO

case class KNNFileParser(iterator: Iterator[String]) extends Iterator[(Int, Array[(Int, Double)])] {

    // Converts an iterator of strings, of the format: 1234,4444 1.021,4445 2.0139
    //                                                 id,neighbor_id distfrom,neighbor_id distfrom
    override def next: (Int, Array[(Int, Double)]) = {

      val entry = iterator.next.split(",")
      val qpId = entry.head.toInt
      val resultSet = entry.tail.map(x => {
        val neighbor = x.split(" ")
        (neighbor.head.toInt, neighbor.last.toDouble)
      })
      (qpId, resultSet)
    }

    override def hasNext: Boolean = this.iterator.hasNext
}
