package benchmark

/**
  * Created by remeeh on 28-03-2017.
  */

import java.io.File
import java.util.concurrent.TimeUnit

import akka.actor.{ActorSystem, Props}
import io.Parser.DisaParser
import lsh.LSHStructure
import measures.Euclidean
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Thread)
class LSHStructureQuery {
  var lsh:LSHStructure = _
  var queryPoints:Iterator[(Int, Array[Float])] = _
  var nextPoint:(Int, Array[Float]) = _
  val rnd = new Random(System.currentTimeMillis())
  var system:ActorSystem = _

  @Setup(Level.Trial)
  def init() = {
    system = ActorSystem("BenchmarkSystem")

    val a1 = system.actorOf(Props[actors.RepetitionHandler], name = "rep1")
    val a2 = system.actorOf(Props[actors.RepetitionHandler], name = "rep2")

    lsh = new LSHStructure(Array(system.actorSelection(a1.path), system.actorSelection(a2.path)))
    lsh.build("../data/descriptors-40000-reduced-128.data", 39290, 1, "Hyperplane", "pq", 1000,16, 128,Euclidean, rnd.nextLong())
    println("Structure Was built!")
  }

  @Setup(Level.Iteration) // (is group of invocations)
  def ite = {
    val points = DisaParser(Source.fromFile(new File("../data/descriptors-40000-reduced-128.data")).getLines(), 128).toIndexedSeq
    rnd.shuffle(points)
    queryPoints = points.toIterator
  }

  @Setup(Level.Invocation) // (Needs to be 100)
  def invo = {
    nextPoint = queryPoints.next
  }
  @TearDown(Level.Trial)
  def td:Unit = {
    val nothing = system.terminate()
  }

  @Benchmark
  def query(bh:Blackhole) : Unit = {
    bh.consume(lsh.query(nextPoint, 10))
  }




}
