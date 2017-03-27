package tools

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by remeeh on 27-03-2017.
  */

object SSQuickSelect {
  def quickSelect(array: Array[Double], n: Int, rand: Random = new Random): Double = {
    val pivot = rand.nextInt(array.length)
    val (left, right) = array.partition(_ < array(pivot))
    if (left.length == n) {
      array(pivot)
    } else if (left.length < n) {
      quickSelect(right, n - left.length, rand)
    } else {
      quickSelect(left, n, rand)
    }
  }

}
