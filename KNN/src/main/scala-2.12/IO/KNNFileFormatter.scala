package IO

import concrete.KNN.{KNearestNeighbors, QueryPointId}

import scala.collection.immutable.HashMap

object KNNFileFormatter {
  /*
   * should output in format:
   *
   * 123,1 1.0,2 2.0,3 3.0
   * 123,1 1.0,2 2.0,3 3.0
   * 123,1 1.0,2 2.0,3 3.0
   */
  def outPut(map: HashMap[QueryPointId, KNearestNeighbors]) : String = {
    map.foldLeft("") {
      (result, entry) =>
        result + entry._1 + {
          entry._2.foldLeft("")((r, n) => r + "," + n._1 + " " + n._2)
        } + "\n"
    }
  }
}
