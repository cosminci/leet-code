package com.leetcode.cosminci._2700

object _2670_FindTheDistinctDiffArray:

  def distinctDifferenceArray(nums: Array[Int]): Array[Int] =
    nums
      .foldLeft(Array.empty[Int], Set.empty[Int], nums.groupMapReduce(identity)(_ => 1)(_ + _)) {
        case ((res, prefix, suffix), num) =>
          val newPrefix = prefix + num
          val newSuffix = suffix.updatedWith(num) {
            case Some(v) if v > 1 => Some(v - 1)
            case _                => None
          }
          (res :+ (newPrefix.size - newSuffix.size), newPrefix, newSuffix)
      }._1
