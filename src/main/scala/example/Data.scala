package example

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Data {
  /*
    type Coll[T] = Array[T]

    def EmptyColl[T: ClassTag]: Coll[T] = Array[T]()

    def CollOf[T: ClassTag](xs: TraversableOnce[T]): Coll[T] = xs.toArray

    def CollFill[T: ClassTag](n: Int)(x: ⇒ T): Coll[T] = Array.fill(n)(x)
  */

  type Coll[T] = Vector[T]

  def EmptyColl[T: ClassTag]: Coll[T] = Vector[T]()

  def CollOf[T: ClassTag](xs: TraversableOnce[T]): Coll[T] = Vector.concat(xs.toSeq)

  def CollFill[T: ClassTag](n: Int)(x: ⇒ T): Coll[T] = Vector.fill(n)(x)


  def interleave[T: ClassTag](what: Coll[Coll[T]]): Coll[T] = {

    val iterators = what.map(_.iterator)

    val repeated = Iterator.continually(iterators)
      .takeWhile(_.exists(_.hasNext))
      .flatten

    repeated.foldLeft(EmptyColl[T]) {
      case (acc, i) => if (i.hasNext) acc :+ i.next() else acc
    }
  }

  def interleaveR[T](what: Coll[Coll[T]])(implicit ev: ClassTag[T]): Coll[T] = {

    @tailrec
    def interleaveRec(what: Coll[Coll[T]], accum: Coll[T] = EmptyColl[T](ev)): Coll[T] = {
      if (what.isEmpty) {
        accum
      } else {
        val nonEmpty = what.filter(_.nonEmpty)
        interleaveRec(nonEmpty.map(_.tail), accum ++ nonEmpty.map(_.head))
      }
    }

    interleaveRec(what)
  }

  def interleaveY[T](what: Vector[Vector[T]], accum: Vector[T] = Vector.empty): Vector[T] = {
    if (what.isEmpty) {
      accum
    } else {
      val (heads, tails) = what
        .filter(_.nonEmpty)
        .foldLeft((accum, Vector.empty[Vector[T]])) {
          case ((head, tails), list) => (head :+ list.head, tails :+ list.tail)
        }

      interleaveY(tails, heads)
    }
  }

  def interleaveZ[T](what: Seq[Seq[T]], accum: Vector[T] = Vector.empty): Seq[T] = {

    val remaining = what.filter(_.nonEmpty)

    if (remaining.isEmpty) {
      accum
    } else {
      interleaveZ(
        remaining.map(_.tail),
        remaining.map(_.head).foldLeft(accum)(_ :+ _)
      )
    }
  }
}
