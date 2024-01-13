package com.leetcode.cosminci._3000

import scala.util.chaining.*

object _2958_LenOfLongestSubarrayWithAtMostKFreq:

  def maxSubarrayLength(nums: Array[Int], k: Int): Int =
    def dfs(l: Int, r: Int, res: Int, f: Map[Int, Int]): Int =
      if r == nums.length then res
      else Iterator
        .iterate((l, inc(f, nums(r)))) { case (l, f) => (l + 1, dec(f, nums(l))) }
        .dropWhile { case (_, f) => f(nums(r)) > k }.next()
        .pipe { case (l, f) => dfs(l, r + 1, res.max(r - l + 1), f) }

    dfs(l = 0, r = 0, res = 0, f = Map.empty.withDefaultValue(0))
  
  private def inc(f: Map[Int, Int], x: Int) = f.updated(x, f(x) + 1)
  private def dec(f: Map[Int, Int], x: Int) = f.updated(x, f(x) - 1)