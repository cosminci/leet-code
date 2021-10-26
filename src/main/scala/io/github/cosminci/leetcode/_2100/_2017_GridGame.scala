package io.github.cosminci.leetcode._2100

object _2017_GridGame:
  def main(args: Array[String]): Unit =
    println(gridGame(Array(Array(2, 5, 4), Array(1, 5, 1))))
    println(gridGame(Array(Array(3, 3, 1), Array(8, 5, 2))))
    println(gridGame(Array(Array(1, 3, 1, 15), Array(1, 3, 3, 1))))

  private def gridGame(grid: Array[Array[Int]]): Long =
    val suffixSums = grid.head.scanRight(0L)(_ + _).drop(1)
    val prefixSums = grid.last.scanLeft(0L)(_ + _).dropRight(1)

    suffixSums.zip(prefixSums).foldLeft(Long.MaxValue) {
      case (minRobot2Points, (suffixSum, prefixSum)) =>
        math.min(minRobot2Points, math.max(suffixSum, prefixSum))
    }
