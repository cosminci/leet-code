package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _486_PredictTheWinner:
  def main(args: Array[String]): Unit =
    println(predictTheWinner(Array(1, 5, 2)))
    println(predictTheWinner(Array(1, 5, 233, 7)))

  private def predictTheWinner(nums: Array[Int]): Boolean =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(left: Int, right: Int): Int =
      if left == right then return nums(left)
      if mem.contains((left, right)) then return mem((left, right))

      val result = math.max(
        nums(left) - dfs(left + 1, right),
        nums(right) - dfs(left, right - 1)
      )

      mem.update((left, right), result)
      result

    dfs(0, nums.length - 1) >= 0
