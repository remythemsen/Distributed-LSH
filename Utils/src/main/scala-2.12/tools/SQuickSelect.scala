package tools

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by remeeh on 27-03-2017.
  */

object SQuickSelect {
  def quickSelect(array: ArrayBuffer[(Int, Double)], n: Int, rand: Random = new Random): Double = {
    val pivot = rand.nextInt(array.length)
    val (left, right) = array.partition(x => x._2 < array(pivot)._2)
    if (left.length == n) {
      array(pivot)._2
    } else if (left.length < n) {
      quickSelect(right, n - left.length, rand)
    } else {
      quickSelect(left, n, rand)
    }
  }

}
