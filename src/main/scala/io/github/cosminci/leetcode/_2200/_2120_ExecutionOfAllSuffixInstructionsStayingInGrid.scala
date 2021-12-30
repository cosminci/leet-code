package io.github.cosminci.leetcode._2200

object _2120_ExecutionOfAllSuffixInstructionsStayingInGrid:
  def main(args: Array[String]): Unit =
    println(executeInstructions(3, Array(0, 1), "RRDDLU").toSeq)

  def executeInstructions(n: Int, startPos: Array[Int], s: String): Array[Int] =
    s.indices.map { i =>
      Iterator
        .iterate((startPos.head, startPos.last, i)) { case (x, y, i) =>
          if i == s.length then (x, y, i + 1)
          else s(i) match
            case 'R' => (x, y + 1, i + 1)
            case 'L' => (x, y - 1, i + 1)
            case 'D' => (x + 1, y, i + 1)
            case 'U' => (x - 1, y, i + 1)
        }
        .drop(1)
        .takeWhile { case (x, y, i) => i <= s.length && x >= 0 && x < n && y >= 0 && y < n }
        .size
    }.toArray
