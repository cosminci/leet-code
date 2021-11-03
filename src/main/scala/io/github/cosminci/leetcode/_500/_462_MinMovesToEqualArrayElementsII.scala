package io.github.cosminci.leetcode._500

object _462_MinMovesToEqualArrayElementsII:
  def main(args: Array[String]): Unit =
    println(minMoves2(Array(1, 2, 3)))
    println(minMoves2(Array(1, 10, 2, 9)))
    println(minMoves2(Array(1, 0, 0, 8, 6)))

  def minMoves2(nums: Array[Int]): Int =
    val median = nums.sortWith(_ < _)(nums.length / 2)
    nums.foldLeft(0)((moves, n) => moves + math.abs(median - n))
