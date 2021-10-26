package io.github.cosminci.leetcode._900

import scala.collection.mutable

object _813_LargestSumOfAverages:
  def main(args: Array[String]): Unit =
    println(largestSumOfAverages(Array(9, 1, 2, 3, 9), 3))
    println(largestSumOfAverages(Array(1, 2, 3, 4, 5, 6, 7), 4))

  private def largestSumOfAverages(nums: Array[Int], k: Int): Double =
    val mem = mutable.Map.empty[(Int, Int, Int), Double]

    def avg(start: Int, end: Int): Double =
      nums.slice(start, end).sum.toDouble / (end - start)

    def dfs(prevEnd: Int, currIdx: Int, budget: Int): Double =
      mem.getOrElseUpdate(
        (prevEnd, currIdx, budget), {
          if currIdx == nums.length then 0
          else if budget == 1 then avg(currIdx, nums.length)
          else
            (avg(prevEnd, currIdx + 1) + dfs(currIdx + 1, currIdx + 1, budget - 1))
              .max(dfs(prevEnd, currIdx + 1, budget))
        }
      )

    dfs(prevEnd = 0, currIdx = 0, budget = k)
