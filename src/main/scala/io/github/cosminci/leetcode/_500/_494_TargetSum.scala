package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _494_TargetSum:
  def main(args: Array[String]): Unit =
    println(findTargetSumWaysTopDown(Array(1, 1, 1, 1, 1), 3))
    println(findTargetSumWaysBottomUp(Array(1, 1, 1, 1, 1), 3))

  /** another way to see it: sum(positive) - sum(negative) == target or sum(p) - sum(n) + sum(p) + sum(n) = target +
    * sum(p) + sum(n) 2 * sum(p) = target + sum(all) sum(p) = (target + sum(all)) / 2
    */
  private def findTargetSumWaysBottomUp(nums: Array[Int], t: Int): Int =
    val sum = nums.sum
    if nums.length == 0 || (t + sum) % 2 == 1 || math.abs(t) > math.abs(sum) then return 0
    val target = (t + sum) / 2

    val dp = Array.ofDim[Int](target + 1)
    dp(0) = 1
    nums.foreach { n =>
      (dp.length - 1 to n by -1).foreach { i =>
        dp(i) += dp(i - n)
      }
    }
    dp.last

  private def findTargetSumWaysTopDown(nums: Array[Int], t: Int): Int =
    if nums.length == 0 then return 0
    if nums.length == 1 then return if math.abs(nums.head) == math.abs(t) then 1 else 0

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(idx: Int, target: Int): Int =
      if idx == nums.length then return if target == 0 then 1 else 0
      if mem.contains((idx, target)) then return mem((idx, target))

      val result = dfs(idx + 1, target = target - nums(idx)) + dfs(idx + 1, target = target + nums(idx))
      mem.update((idx, target), result)
      result

    dfs(0, t)
