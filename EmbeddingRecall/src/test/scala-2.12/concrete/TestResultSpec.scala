package concrete

import org.scalatest.{FlatSpec, Matchers}

class TestResultSpec extends FlatSpec with Matchers {
  "toString" should "create proper string that corresponds to input" in {
    val resultSet = Vector(
      (0.71, 0.01),
      (0.8, 0.05),
      (1.0, 0.1),
      (1.0, 0.2)
    )
    val dims = 128
    val tr = TestResult(dims, resultSet)

    val shouldBe = {
      dims+"d " +
        resultSet(0)._1 + " " +
        resultSet(1)._1 + " " +
        resultSet(2)._1+ " " +
        resultSet(3)._1
    }

    assert(tr.toString == shouldBe)
  }
}
