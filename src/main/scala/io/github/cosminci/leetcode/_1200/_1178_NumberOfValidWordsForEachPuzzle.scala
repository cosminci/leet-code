package io.github.cosminci.leetcode._1200

object _1178_NumberOfValidWordsForEachPuzzle:
  def main(args: Array[String]): Unit =
    println(
      findNumOfValidWords(
        Array("aaaa", "asas", "able", "ability", "actt", "actor", "access"),
        Array("aboveyz", "abrodyz", "abslute", "absoryz", "actresz", "gaswxyz")
      )
    )

  def findNumOfValidWords(words: Array[String], puzzles: Array[String]): List[Int] =
    def bitmask(s: String): Int =
      s.toSet.foldLeft(0)((bitmask, char) => bitmask | (1 << (char - 'a')))

    val wordBitmaskCounts = words.groupBy(bitmask).mapValues(_.length).toMap.withDefaultValue(0)

    puzzles.map { puzzle =>
      val first = 1 << (puzzle.head - 'a')
      val mask  = bitmask(puzzle.tail)

      def count(submask: Int): Int =
        if submask == 0 then 0
        else wordBitmaskCounts(submask | first) + count((submask - 1) & mask)

      wordBitmaskCounts(first) + count(mask)
    }.toList
