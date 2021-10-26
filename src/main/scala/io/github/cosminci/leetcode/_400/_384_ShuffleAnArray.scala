package io.github.cosminci.leetcode._400

import scala.util.Random

object _384_ShuffleAnArray:

  class Solution(_nums: Array[Int]):
    private val original = _nums.zipWithIndex

    def reset(): Array[Int] =
      original.foreach { case (n, i) => _nums(i) = n }
      _nums

    def shuffle(): Array[Int] =
      (_nums.length - 1 to 1 by -1).foreach { i =>
        val j   = Random.nextInt(i + 1)
        val tmp = _nums(j)
        _nums(j) = _nums(i)
        _nums(i) = tmp
      }
      _nums
