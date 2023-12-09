package labs.task2

import scala.math.BigDecimal.double2bigDecimal

object Task2 {
  private val step: BigDecimal = 0.25

  def memoizedFunction(f: BigDecimal => BigDecimal): Int => BigDecimal = {
    arg => (f(arg * step) + f((arg - 1) * step)) / 2 * step
  }


  def integrateWithMemoize(f: Int => BigDecimal): Int => BigDecimal = {
    end => Range.inclusive(1, end).foldLeft(BigDecimal(0.0))((acc, elem) => acc + f(elem))
  }

  def integrateWithMemoizeEndlessStream(f: Int => BigDecimal): Int => BigDecimal = {
    LazyList.from(1).map(f).scan(BigDecimal(0.0))(_ + _)
  }


  def main(args: Array[String]): Unit = {
    val trapeziumNumber: Int = 40
    val res1 = integrateWithMemoize(memoizedFunction(x => Math.exp(x.toDouble) * 2))
    val res2 = integrateWithMemoizeEndlessStream(memoizedFunction(x => Math.exp(x.toDouble) * 2))
    println(res1(trapeziumNumber))
    println(res2(trapeziumNumber))
  }
}
