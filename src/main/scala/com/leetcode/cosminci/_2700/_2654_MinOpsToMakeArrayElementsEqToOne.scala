package com.leetcode.cosminci._2700

import com.leetcode.cosminci.utils.gcd
import scala.util.chaining.*

object _2654_MinOpsToMakeArrayElementsEqToOne:

  def minOperations(nums: Array[Int]): Int =
    if nums.count(_ == 1) > 0 then nums.length - nums.count(_ == 1)
    else (0 until nums.length - 1)
      .foldLeft(Int.MaxValue) { (diff, i) =>
        (i + 1 until nums.length)
          .foldLeft(diff, nums(i)) { case ((diff, prevGcd), j) =>
            val newGcd = gcd(prevGcd, nums(j))
            if newGcd == 1 then (diff.min(j - i), newGcd) else (diff, newGcd)
          }.pipe { case (diff, _) => diff }
      }.pipe(diff => if diff == Int.MaxValue then -1 else diff + nums.length - 1)
