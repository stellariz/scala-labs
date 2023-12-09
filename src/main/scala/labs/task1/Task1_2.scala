package labs.task1

import scala.annotation.tailrec

object Task1_2 {
  def fillWord(word: String, alphabet: List[String]): List[String] = {
    @tailrec
    def fillWordHelper(result: List[String], alphabet: List[String]): List[String] = {
      if (alphabet.isEmpty)
        result
      else
        fillWordHelper(if (alphabet.head == word.last.toString) result else  (word + alphabet.head) +: result,
          alphabet.tail)
    }

    fillWordHelper(Nil, alphabet)
  }

  def fillResult(words: List[String], alphabet: List[String]): List[String] = {
    @tailrec
    def fillResultHelper(words: List[String], result: List[String]): List[String] = {
      if (words.isEmpty)
        result
      else
        fillResultHelper(words.tail, result ++ fillWord(words.head, alphabet))
    }

    if (words.isEmpty)
      alphabet
    else
      fillResultHelper(words, Nil)
  }

  def generatePermutations(n: Int, alphabet: List[String]): List[String] = {
    @tailrec
    def permutationsHelper(n: Int, result: List[String]): List[String] = {
      if (n > 0)
        permutationsHelper(n - 1, fillResult(result, alphabet))
      else
        result
    }

    permutationsHelper(n, Nil)
  }

  def main(args: Array[String]): Unit = {
    val result = generatePermutations(3, List("a", "b", "b", "d"))
    println(result)
  }

}
