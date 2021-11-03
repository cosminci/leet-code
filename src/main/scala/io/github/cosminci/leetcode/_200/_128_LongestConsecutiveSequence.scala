package io.github.cosminci.leetcode._200

object _128_LongestConsecutiveSequence:
  def longestConsecutive(input: Array[Int]): Int =
    if input.isEmpty then return 0
    val nums = input.toSet
    var max  = 1
    nums.foreach { n =>
      if !nums.contains(n - 1) then
        var seq = 1
        while nums.contains(n + seq) do seq += 1
        max = math.max(max, seq)
    }
    max
