package io.github.cosminci.leetcode._100

object _16_ThreeSumClosest:

  def threeSumClosest(nums: Array[Int], target: Int): Int =
    nums.sortInPlace()
    (0 until nums.length - 2).foldLeft(if target < 0 then Int.MinValue else Int.MaxValue) { (closest, i) =>
      @annotation.tailrec
      def dfs(j: Int, k: Int, closest: Int): Int =
        if j >= k then closest
        else
          val sum        = nums(i) + nums(j) + nums(k)
          val newClosest = if (target - sum).abs < (target - closest).abs then sum else closest
          if sum > target then dfs(j, k - 1, newClosest) else dfs(j + 1, k, newClosest)

      dfs(j = i + 1, k = nums.length - 1, closest)
    }
