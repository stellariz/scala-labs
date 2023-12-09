package labs.task2

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode, OutputTimeUnit, Scope, State, Warmup}

import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@Fork(value = 2)
@Warmup(iterations = 2)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class BenchmarkTask2 {
  private val trapeziumNumber: Int = 40

  @Benchmark
  def testDefaultVersion(): BigDecimal =
    Task2.integrateWithMemoize(Task2.memoizedFunction(x => Math.exp(x.toDouble) * 2))(trapeziumNumber)

  @Benchmark
  def testLazyVersion(): BigDecimal =
    Task2.integrateWithMemoizeEndlessStream(Task2.memoizedFunction(x => Math.exp(x.toDouble) * 2))(trapeziumNumber)
}
