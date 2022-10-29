package com.leetcode.cosminci._1300

object _1220_CountVowelPermutations:

  def countVowelPermutation(n: Int): Int =
    ((1 until n)
      .foldLeft(Array.fill[Long](5)(1)) { (dp, _) =>
        Array(
          dp(1) + dp(2) + dp(4),
          dp(0) + dp(2),
          dp(1) + dp(3),
          dp(2),
          dp(2) + dp(3)
        ).map(_ % 1_000_000_007)
      }
      .sum % 1_000_000_007).toInt
