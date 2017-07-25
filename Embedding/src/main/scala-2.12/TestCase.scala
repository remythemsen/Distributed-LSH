import io.Parser.DisaParser
import measures.Distance

case class TestCase[A](
                    distance:Either[Throwable, Distance[A]],
                    data:Either[Throwable,DisaParser[A]],
                    queries:Either[Throwable, DisaParser[A]],
                    K:Either[Throwable, Int],
                    dataSetSize:Either[Throwable, Int],
                    querySetSize:Either[Throwable, Int],
                    dimensions:Either[Throwable, Int]
                   ) {
}

