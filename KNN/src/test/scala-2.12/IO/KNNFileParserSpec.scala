package IO

import org.scalatest.{FlatSpec, Matchers}

class KNNFileParserSpec extends FlatSpec with Matchers {
  "next" should "provide correct output" in {
    val input = Iterator(
      "0,0 1.0,1 2.0,2 3.0",
      "1,0 2.0,1 4.0,2 6.0",
      "2,0 3.0,1 6.0,2 9.0"
    )

    val parser = KNNFileParser(input)

    parser.zipWithIndex.foreach(i => {
      assert(i._1._1 == i._2)
      i._1._2.zipWithIndex.foreach(j => {
        assert(j._1._1 == j._2)
        val dValue = j._1._2
        val iValue = i._1._1
        val jValue = j._1._1
        val shouldBe = (iValue+1)*(jValue+1)
        assert(dValue == shouldBe)
      })
    })
  }

}
