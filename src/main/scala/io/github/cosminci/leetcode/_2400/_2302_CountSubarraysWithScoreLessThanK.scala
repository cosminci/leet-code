package io.github.cosminci.leetcode._2400

object _2302_CountSubarraysWithScoreLessThanK:

  def countSubarrays(nums: Array[Int], k: Long): Long =
    @annotation.tailrec
    def dfs(left: Int, right: Int, sum: Long, count: Long): Long =
      if right == nums.length then count
      else
        val (newSum, newLeft) = Iterator
          .iterate((sum + nums(right), left)) { case (sum, left) => (sum - nums(left), left + 1) }
          .dropWhile { case (sum, left) => sum * (right - left + 1) >= k }
          .next()
        dfs(newLeft, right + 1, newSum, count + (right - newLeft + 1))

    dfs(left = 0, right = 0, sum = 0, count = 0)
