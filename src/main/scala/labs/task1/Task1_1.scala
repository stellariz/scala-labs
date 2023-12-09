package labs.task1

import scala.collection.mutable.ArrayBuffer

object Task1_1 {
  def main(args: Array[String]): Unit = {

    val stringLength: Int = 5
    val acc: ArrayBuffer[String] = ArrayBuffer[String]()

    val alphabet: List[String] = List("a", "b", "c", "d")

    def permute(currString: String, currSymb: String): Unit = {
      if (currString.length == stringLength) {
        acc += currString
        return
      }

      for (letter <- alphabet) {
        if (letter != currSymb) {
          permute(currString + letter, letter)
        }
      }
    }

    permute("", "")
    println(acc.map(buf => buf.mkString("")))

  }
}