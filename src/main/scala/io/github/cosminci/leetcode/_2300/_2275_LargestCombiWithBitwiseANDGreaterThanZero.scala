package io.github.cosminci.leetcode._2300

object _2275_LargestCombiWithBitwiseANDGreaterThanZero:

  def largestCombination(candidates: Array[Int]): Int =
    (0 until 24).map(bit => candidates.count(c => (c >> bit & 1) == 1)).max
