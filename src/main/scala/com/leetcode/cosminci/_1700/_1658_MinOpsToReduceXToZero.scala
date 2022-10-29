package com.leetcode.cosminci._1700

import scala.collection.mutable

object _1658_MinOpsToReduceXToZero:

  def minOperations(nums: Array[Int], x: Int): Int =
    val prefixSums = nums.scanLeft(0)(_ + _).zipWithIndex.toMap
    val target     = nums.sum - x

    if target < 0 then return -1

    val maxLength = prefixSums.foldLeft(Int.MinValue) { case (maxLength, (psum, i)) =>
      prefixSums.get(psum + target) match
        case None    => maxLength
        case Some(j) => maxLength.max(j - i)
    }
    if maxLength != Int.MinValue then nums.length - maxLength else -1
