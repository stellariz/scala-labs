package labs.task3

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode, OutputTimeUnit, Scope, State, Warmup}

import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@Fork(value = 1)
@Warmup(iterations = 1)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class BenchmarkTask3  {
  private val testList: List[Int] = List.tabulate(5000)(elem => elem % 6)
  private val testLazyLust: LazyList[Int] = LazyList.tabulate(5000)(elem => elem % 6)

  private val batchSize: Int = 10

  @Benchmark
  def testDefaultVersionListFilter(): Unit =
    testList.filter(x => {
      Thread.sleep(10)
      x == 1
    })

  @Benchmark
  def testParallelVersionListFilter(): Unit =
    Task3.parallelFilter(testList, batchSize, (x: Int) => {
      Thread.sleep(10)
      x == 1
    })

  @Benchmark
  def testDefaultVersionLazyListFilter(): Unit =
    testLazyLust.filter(x => {
      Thread.sleep(10)
      x == 1
    }).toList

  @Benchmark
  def testParallelVersionLazyListFilter(): Unit =
    Task3.parallelFilterLazyList(testLazyLust, batchSize, (x: Int) => {
      Thread.sleep(10)
      x == 1
    })
}
