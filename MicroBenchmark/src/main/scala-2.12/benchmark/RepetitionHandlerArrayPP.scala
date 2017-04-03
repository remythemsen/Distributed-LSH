package benchmark

import java.io.File
import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import io.Parser.DisaParser
import measures.Euclidean
import messages.{InitRepetition, Query}
import org.openjdk.jmh.annotations.{OutputTimeUnit, _}
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class RepetitionHandlerArrayPP {

  implicit val timeout = Timeout(10.hours)

  var rnd:Random = new Random(System.currentTimeMillis())
  var queryPoints:Iterator[Array[Float]] = _
  var nextPoint:Array[Float] = _
  var system:ActorSystem = _
  var a1:ActorRef = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    rnd = new Random(System.currentTimeMillis())
    system = ActorSystem("BenchmarkSystem")
    a1 = system.actorOf(Props[actors.RepetitionHandlerArrayPP], name = "rep1")


    val ready = a1 ? InitRepetition("../data/descriptors-40000-reduced-128.data", 39290, 1, "hyperplane", 16, 128, Euclidean, rnd.nextLong)
    Await.result(ready, timeout.duration)

  }
  @Setup(Level.Iteration)
  def getQueries() = {
    val points = DisaParser(Source.fromFile(new File("../data/descriptors-40000-reduced-128.data")).getLines(), 128).map(x => x._2).toIndexedSeq
    rnd.shuffle(points)
    queryPoints = points.toIterator
  }

  @Setup(Level.Invocation)
  def pickRndVectorKey(): Unit = {
    nextPoint = queryPoints.next
  }

  @TearDown(Level.Trial)
  def td:Unit = {
    val nothing = system.terminate()
  }

  // Remember to read from variable, and consume result by blackhole (avoid dead code eli)
  @Benchmark
  def query(bh:Blackhole):Unit = {
    val cands:Future[Any] = a1 ? Query(nextPoint, 10)
    bh.consume(Await.result(cands, timeout.duration).asInstanceOf[Array[(Int, Double)]])
  }
}
