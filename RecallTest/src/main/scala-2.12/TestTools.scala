class TestTools {
  def average(sum:Double, length:Int) : Double = {
    sum / length
  }
  def average(seq:Seq[Double]) : Double = {
    seq.sum / seq.length
  }
  def stdDeviation(seq:Seq[Double]) : Double = {
    Math.sqrt(variance(seq))
  }
  def variance(seq: Seq[Double]) : Double = {
    val avg = average(seq)
    seq.map(x => math.pow(x - avg, 2)).sum / seq.length
  }
  def timer[R](r: => R): Double = {
    val now = System.nanoTime
    r
    val time = System.nanoTime - now
    time
  }
  def punish(optSum:Double, lshSum:Double, missing:Int):Double = {
    10*optSum*missing
  }

  def queryRecalls(optimal:Seq[(Int,Double)], lshResult:Seq[(Int,Double,Int)], knn:Int) : Double = {
    val optSum:Double = optimal.map(_._2).sum
    var qResSum = lshResult.map(x => x._2).sum
    if(lshResult.size < knn) {
      // Punishment
      qResSum += punish(optSum, qResSum, knn-lshResult.length)
    }

    optSum / qResSum
  }

}
