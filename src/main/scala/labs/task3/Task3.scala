package labs.task3

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


object Task3 {
  implicit class RichList[A](list: List[A]) {
    def parallel_filter(batch_size: Int, f: A => Boolean): List[A] = {
      def parallelFilterHelper(list: List[A]): Future[List[A]] = Future {
        list.filter(f)
      }

      val myFutureList = Future.traverse(list.grouped(batch_size))(listPart => parallelFilterHelper(listPart))
      Await.result(myFutureList, Duration.Inf).reduce(_ ++ _)
    }
  }

  def parallelFilter[A](dirtyList: List[A], batch_size: Int, f: A => Boolean): List[A] = {
    def parallelFilterHelper(list: List[A]): Future[List[A]] = Future {
      list.filter(f)
    }

    val myFutureList = Future.traverse(dirtyList.grouped(batch_size))(listPart => parallelFilterHelper(listPart))
    Await.result(myFutureList, Duration.Inf).reduce(_ ++ _)
  }

  def parallelFilterLazyList[A](dirtyLazyList: LazyList[A], batch_size: Int, f: A => Boolean): List[A] = {
    def parallelFilterLazyListHelper(list: LazyList[A]): Future[List[A]] = Future {
      list.filter(f).toList
    }
  
    val myFutureList = Future.traverse(dirtyLazyList.grouped(batch_size).toList)(listPart => parallelFilterLazyListHelper(listPart))
    Await.result(myFutureList, Duration.Inf).reduce(_ ++ _)
  }

  def main(args: Array[String]): Unit = {
  }
}
