package tools

import scala.util.Random

/**
  * Created by remeeh on 27-03-2017.
  */

object SVQuickSelect {
  def quickSelect(vector: Vector[(Int, Double)], n: Int, rand: Random = new Random): Double = {
    val pivot = rand.nextInt(vector.size)
    val (left, right) = vector.partition(x => x._2 < vector(pivot)._2)
    if (left.length == n) {
      vector(pivot)._2
    } else if (left.length < n) {
      quickSelect(right, n - left.length, rand)
    } else {
      quickSelect(left, n, rand)
    }
  }

}
