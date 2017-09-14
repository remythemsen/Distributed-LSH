package concrete

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Random, Success}

class TestCaseManagerSpec extends FlatSpec with Matchers {
  "generateCases " should "produce iterator of testcases in equal length to its parameter: cases" in {
    val file = Files.write(Paths.get("generateCasesTestfile1"), "".getBytes(StandardCharsets.UTF_8))
    val rnd = new Random(System.currentTimeMillis())
    val size = rnd.nextInt(50)
    val input = {
      val item = "generateCasesTestfile1 generateCasesTestfile1 50"
      Iterator.fill[String](size)(item)
    }
    val inputSize = size
    val cases = TestCaseManager.generateCases(input)
    try {
      assert(cases.size == inputSize)
    } catch {
      case e: Exception => throw e
    } finally {
      file.toFile.delete()
    }
  }

  "generateCases " should "return a list containing a failure on wrong testcase input" in {
    val input = "filedoesnotexist neitherdoesthis 50"
    val cases = TestCaseManager.generateCases(Iterator(input))
    cases.map({
      case Failure(thrown) => throw thrown
      case Success(tc) => TestCase
    })

  }
}
