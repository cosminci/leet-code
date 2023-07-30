package com.leetcode.cosminci._2700

import com.leetcode.cosminci.utils

object _2670_FindTheDistinctDiffArray:

  def distinctDifferenceArray(nums: Array[Int]): Array[Int] =
    nums
      .foldLeft(Array.empty[Int], Set.empty[Int], nums.groupMapReduce(identity)(_ => 1)(_ + _)) {
        case ((res, prefix, suffix), num) =>
          val newPrefix = prefix + num
          val newSuffix = utils.decrementCounter(suffix, num)
          (res :+ (newPrefix.size - newSuffix.size), newPrefix, newSuffix)
      }._1
