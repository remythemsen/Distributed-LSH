package concrete

case class TestResult(numOfComponents:Int, results:Vector[(Double, Double)]) {
  override def toString : String = {
    numOfComponents.toString+"d" + results.foldLeft("") {
      (s, r) => s + " " + r._1
    }
  }
}

