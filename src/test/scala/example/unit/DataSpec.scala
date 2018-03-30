package example.unit

import example.Data._
import example.Util._
import org.scalatest.{FlatSpec, Matchers}

class DataSpec extends FlatSpec with Matchers {

  behavior of "interleave"

  def testData(n: Int): Coll[Coll[Int]] = CollOf(List(CollFill(n)(1), CollFill(n)(2)))
  def testData2(n: Int): Coll[Coll[Int]] = CollOf((0 to n).map(i ⇒ CollFill(i)(i)))

  it should "produce correct results" in {
    interleave(testData2(4)) shouldEqual Seq(1, 2, 3, 4, 2, 3, 4, 3, 4, 4)
    interleaveR(testData2(4)) shouldEqual Seq(1, 2, 3, 4, 2, 3, 4, 3, 4, 4)
    interleaveY(testData2(4)) shouldEqual Seq(1, 2, 3, 4, 2, 3, 4, 3, 4, 4)
    interleaveZ(testData2(4)) shouldEqual Seq(1, 2, 3, 4, 2, 3, 4, 3, 4, 4)
  }

  it should "run on a large array" in {
    val n1 = 1000000

    val elapsed1 = time(interleave(testData(n1)).length)._2 / 1000000000.0
    println(s"iterator version with $n1 takes $elapsed1 s")

    val n2 = 1000000
    val elapsed2 = time(interleaveR(testData(n2)).length)._2 / 1000000000.0
    println(s"recursive version with $n2 takes $elapsed2 s")

    val n3 = 1000000
    val elapsed3 = time(interleaveY(testData(n3)).length)._2 / 1000000000.0
    println(s"recursive version with $n3 takes $elapsed3 s")

    val n4 = 1000000
    val elapsed4 = time(interleaveZ(testData(n4)).length)._2 / 1000000000.0
    println(s"recursive version with $n4 takes $elapsed4 s")
  }
}
