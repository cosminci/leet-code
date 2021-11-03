package io.github.cosminci.leetcode._900

object _828_CountUniqueCharactersOfAllSubstringsOfAGivenString:
  def main(args: Array[String]): Unit =
    println(uniqueLetterString("LEETCODE"))

  def uniqueLetterString(s: String): Int =
    val prevTwoIndices = Array.fill(26)((-1, -1))
    val result         = Array.ofDim[Int](s.length + 1)

    s.zipWithIndex.foreach { case (char, curIdx) =>
      val (secondLast, last) = prevTwoIndices(char - 'A')
      result(curIdx + 1) = 1 + result(curIdx) + (curIdx - 1 - secondLast) - (secondLast - last)
      prevTwoIndices(char - 'A') = (curIdx, secondLast)
    }

    result.sum
