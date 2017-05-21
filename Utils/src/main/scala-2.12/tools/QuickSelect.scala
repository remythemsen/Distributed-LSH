package tools

/**
  * Heavily inspired from
  * http://blog.teamleadnet.com/2012/07/quick-select-algorithm-find-kth-element.html
  *
  */
class QuickSelect {
  def selectKthDist(data:CandSet, k:Int, untilIncluding:Int):Double = {

    var from = 0
    var to = untilIncluding

    // if from and to is equal, it's the kth element
    while (from < to) {
      var r = from
      var w = to
      var pivot = data.dists.getDouble((r + w) / 2)

      // stop run if r and w meets
      while (r < w) {
        if (data.dists.getDouble(r) >= pivot) {
          var tmpDist = data.dists.getDouble(w)
          var tmpId = data.ids.getInt(w)
          data.dists.set(w, data.dists.getDouble(r))
          data.ids.set(w, data.ids.getInt(r))
          data.dists.set(r, tmpDist)
          data.ids.set(r, tmpId)
          w -= 1
        } else {
          r += 1
        }
      }

      // if r is incremented then we need to decrement one
      if (data.dists.getDouble(r) > pivot) {
        r -= 1
      }

      if (k <= r) {
        to = r
      } else {
        from = r + 1
      }
    }
    data.dists.getDouble(k)
  }
}
