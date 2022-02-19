package io.github.cosminci.leetcode._1700

import scala.collection.immutable.TreeSet

object _1675_MinimizeDeviationInArray:
  def main(args: Array[String]): Unit =
    println(minimumDeviation(Array(4, 1, 5, 20, 3)))

  def minimumDeviation(nums: Array[Int]): Int =
    @annotation.tailrec
    def dfs(maxNums: TreeSet[Int], minDiff: Int, minN: Int): Int =
      if maxNums.last % 2 == 1 then minDiff.min(maxNums.last - minN)
      else
        dfs(
          maxNums - maxNums.max + maxNums.max / 2,
          minDiff.min(maxNums.max - minN),
          minN.min(maxNums.max / 2)
        )

    val maxNums = TreeSet.from(nums.map(n => if n % 2 == 0 then n else n * 2))
    dfs(maxNums, minDiff = Int.MaxValue, minN = maxNums.min)
