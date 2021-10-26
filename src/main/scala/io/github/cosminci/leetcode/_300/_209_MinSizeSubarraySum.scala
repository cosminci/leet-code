package io.github.cosminci.leetcode._300

object _209_MinSizeSubarraySum {
  def main(args: Array[String]): Unit = {
    println(minSubArrayLen(11, Array(1, 2, 3, 4, 5)))
    println(minSubArrayLen(7, Array(2, 3, 1, 2, 4, 3)))
  }
  def minSubArrayLen(target: Int, nums: Array[Int]): Int = {
    var l = 0
    var rollingSum = 0
    var minWindow = Int.MaxValue
    nums.indices.foreach { r =>
      if (nums(r) >= target) return 1
      rollingSum += nums(r)
      while (rollingSum >= target) {
        minWindow = math.min(minWindow, r - l + 1)
        rollingSum -= nums(l)
        l += 1
      }
    }
    if (minWindow == Int.MaxValue) 0 else minWindow
  }
}
