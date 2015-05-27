package calculator

import scala.math.sqrt
object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal{b() * b() - 4 * a() * c() }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal{
    if (delta()< 0)
      Set()
    else {
      Set((-1 * b()+ sqrt(delta()))/(2 * a()), (-1 * b() - sqrt(delta()))/(2 * a()))
    }
  }
}
