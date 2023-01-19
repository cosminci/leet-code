package com.leetcode.cosminci._1000

object _974_SubarraySumsDivisibileByK:

  def subarraysDivByK(nums: Array[Int], k: Int): Int =
    nums.foldLeft(Map(0 -> 1).withDefaultValue(0), 0, 0) {
      case ((counts, currSum, result), n) =>
        val newSum = math.floorMod(currSum + n % k, k)
        (counts.updated(newSum, counts(newSum) + 1), newSum, result + counts(newSum))
      }._3
