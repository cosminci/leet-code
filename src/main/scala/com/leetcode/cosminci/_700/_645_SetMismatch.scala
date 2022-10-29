package com.leetcode.cosminci._700

import scala.util.chaining.*

object _645_SetMismatch:
  def findErrorNums(nums: Array[Int]): Array[Int] =
    nums
      .foldLeft(Set.from(1 to nums.length), 0) { case ((left, duplicate), n) =>
        if left.contains(n) then (left - n, duplicate) else (left, n)
      }
      .pipe { case (missing, duplicate) => Array(duplicate) ++ missing }
