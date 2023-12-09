package labs.task1

import scala.annotation.tailrec

object Task1_4 {

  def fillWord(words: List[String], alphabet: List[String]): List[String] = {
    if (words.isEmpty)
      alphabet
    else
      words.map(it => alphabet.filter(_ != it.last.toString).map(letter => it + letter)).reduce(_ ++ _)
  }

  @tailrec
  def generatePermutations(n: Int, alphabet: List[String], acc: List[String]): List[String] = {
    if (n > 0)
      generatePermutations(n - 1, alphabet, fillWord(acc, alphabet))
    else
       acc
  }

  def main(args: Array[String]): Unit = {
    val alphabet: List[String] = List("a", "b", "c", "d")
    val stringLength: Int = 3
    println(generatePermutations(stringLength, alphabet, Nil))
  }
}
