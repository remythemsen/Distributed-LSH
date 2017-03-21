package messages

case class QueryResult(resSet:Seq[(Int,Double)], sizeOfRawSet:Int)


