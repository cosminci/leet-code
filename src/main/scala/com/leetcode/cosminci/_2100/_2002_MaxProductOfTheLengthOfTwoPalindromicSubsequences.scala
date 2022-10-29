package com.leetcode.cosminci._2100

import com.leetcode.cosminci.utils

object _2002_MaxProductOfTheLengthOfTwoPalindromicSubsequences:
  def main(args: Array[String]): Unit =
    println(maxProduct("bb"))
    println(maxProduct("leetcodecom"))
    println(maxProduct("accbcaxxcxx"))

  def maxProduct(s: String): Int =
    def dfs(idx: Int, ss1: String, ss2: String): Int =
      if idx == s.length then
        return if utils.isPalindrome(ss1) && utils.isPalindrome(ss2) then ss1.length * ss2.length else 0

      math.max(
        dfs(idx + 1, ss1, ss2),
        math.max(
          dfs(idx + 1, ss1.appended(s(idx)), ss2),
          dfs(idx + 1, ss1, ss2.appended((s(idx))))
        )
      )

    dfs(0, "", "")
