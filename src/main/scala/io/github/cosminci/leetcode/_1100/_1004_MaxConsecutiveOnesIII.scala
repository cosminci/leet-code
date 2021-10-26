package io.github.cosminci.leetcode._1100

object _1004_MaxConsecutiveOnesIII:

  def main(args: Array[String]): Unit =
    println(longestOnes(Array(0, 0, 1, 1, 1, 0, 0), 0))

  def longestOnes(nums: Array[Int], k: Int): Int =
    var windowStart = 0
    var windowEnd   = 0
    var tokensLeft  = k
    var max         = 0

    while windowEnd != nums.length do
      if nums(windowEnd) == 1 then
        windowEnd += 1
        max = math.max(windowEnd - windowStart, max)
      else if tokensLeft > 0 then
        windowEnd += 1
        tokensLeft -= 1
        max = math.max(windowEnd - windowStart, max)
      else
        if nums(windowStart) == 0 then tokensLeft = math.min(k, tokensLeft + 1)
        windowStart += 1
        if windowEnd < windowStart then windowEnd = windowStart

    max
