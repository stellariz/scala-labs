package labs.task1

object Task1_3 {
  def myMap[A, B](coll: List[A], f: A => B): List[B] = {
    coll.foldRight(List.empty[B]) { (v, acc) => f(v) +: acc }
  }


  def myFilter[A](coll: List[A], f: A => Boolean): List[A] = {
    coll.foldRight(List.empty[A]) { (v, acc) =>
      if (f(v))
        v +: acc
      else
        acc
    }
  }

  def main(args: Array[String]): Unit = {
    val test: List[Int] = List(1, 2, 3, 4)
    val mappedList: List[String] = myMap(test, (_: Int) => "hello_world")
    val filteredList: List[Int] = myFilter(test, (x: Int) => x == 2 || x == 3)
    println(mappedList)
    println(filteredList)
  }
}
