package com.leetcode.cosminci._2800

import scala.util.chaining.*

object _2763_SumOfImbalanceNumsOfAllSubarrays:

  def sumImbalanceNumbers(nums: Array[Int]): Int =
    nums.indices.foldLeft(0) { (res, i) =>
      (i + 1 until nums.length).foldLeft(res, Set(nums(i)), 0) { case ((res, seen, sum), j) =>
        if seen.contains(nums(j)) then (res + sum, seen, sum)
        else
          val prevContrib = if seen.contains(nums(j) - 1) then -1 else 0
          val nextContrib = if seen.contains(nums(j) + 1) then -1 else 0
          val newSum      = sum + 1 + prevContrib + nextContrib
          (res + newSum, seen + nums(j), newSum)
      }.pipe { case (res, _, _) => res }
    }
