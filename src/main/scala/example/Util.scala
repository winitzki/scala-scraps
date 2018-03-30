package example

object Util {
  def time[T](x: ⇒ T): (T, Long) = {
    val initTime = System.nanoTime()
    val result = x
    val elapsed = System.nanoTime() - initTime
    (result, elapsed)
  }
}
