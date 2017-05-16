package tools

import it.unimi.dsi.fastutil.doubles.DoubleArrayList

/**
  * Heavily inspired from
  * http://blog.teamleadnet.com/2012/07/quick-select-algorithm-find-kth-element.html
  *
  */
object QuickSelect {
  def selectKthDist(data:DoubleArrayList, k:Int, until:Int):Double = {
    var from = 0
    var to = until

    // if from and to is equal, it's the kth element
    while (from < to) {
      var r = from
      var w = to
      var pivot = data.getDouble((r + w) / 2)

      // stop run if r and w meets
      while (r < w) {
        if (data.getDouble(r) >= pivot) {
          var tmp = data.getDouble(w)
          data.set(w, data.getDouble(r))
          data.set(r, tmp)
          w -= 1
        } else {
          r += 1
        }
      }

      // if r is incremented then we need to decrement one
      if (data.getDouble(r) > pivot) {
        r -= 1
      }

      if (k <= r) {
        to = r
      } else {
        from = r + 1
      }
    }
    data.getDouble(k)
  }
}
