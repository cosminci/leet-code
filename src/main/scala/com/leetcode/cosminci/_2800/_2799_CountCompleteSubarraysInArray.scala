package com.leetcode.cosminci._2800

import com.leetcode.cosminci.utils

import scala.util.chaining.*

object _2799_CountCompleteSubarraysInArray:

  def countCompleteSubarrays(nums: Array[Int]): Int =
    val n      = nums.length
    val target = nums.distinct.length

    @annotation.tailrec
    def dfs(l: Int, r: Int, res: Int, freqs: Map[Int, Int]): Int =
      if l == n then res
      else if r < n && freqs.size < target then
        dfs(l, r + 1, res, freqs.updated(nums(r), freqs.getOrElse(nums(r), 0) + 1))
      else
        val newFreqs = utils.decrementCounter(freqs, nums(l))
        val newRes   = if freqs.size < target then res else res + n - r + 1
        dfs(l + 1, r, newRes, newFreqs)

    dfs(l = 0, r = 0, res = 0, freqs = Map.empty)
