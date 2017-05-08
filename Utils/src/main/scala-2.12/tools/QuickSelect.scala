package tools

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Heavily inspired from
  * http://blog.teamleadnet.com/2012/07/quick-select-algorithm-find-kth-element.html
  *
  */
object QuickSelect {
  def selectKthDist(data:ArrayBuffer[(Int, Double)], k:Int, rnd:Random = new Random(System.currentTimeMillis())):Double = {
    var from = 0
    var to = data.size - 1

    // if from and to is equal, it's the kth element
    while (from < to) {
      var r = from
      var w = to
      var pivot = data((r + w) / 2)

      // stop run if r and w meets
      while (r < w) {
        if (data(r)._2 >= pivot._2) {
          var tmp = data(w)
          data(w) = data(r)
          data(r) = tmp
          w -= 1
        } else {
          r += 1
        }
      }

      // if r is incremented then we need to decrement one
      if (data(r)._2 > pivot._2) {
        r -= 1
      }

      if (k <= r) {
        to = r
      } else {
        from = r + 1
      }
    }
    data(k)._2
  }
}
