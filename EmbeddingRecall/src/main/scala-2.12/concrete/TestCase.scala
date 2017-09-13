package concrete

case class TestCase(
                       reducedDataParser:Iterator[(Int, Array[Double])],
                       reducedQueryParser:Iterator[(Int, Array[Double])],
                       knn:Int,
                       dimensions:Int
                     ) {
}
