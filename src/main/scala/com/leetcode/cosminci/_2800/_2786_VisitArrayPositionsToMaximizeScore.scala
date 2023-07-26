package com.leetcode.cosminci._2800

import scala.util.chaining.*

object _2786_VisitArrayPositionsToMaximizeScore:

  def maxScore(nums: Array[Int], x: Int): Long =
    val even = nums.head.toLong - (if nums.head % 2 == 0 then 0 else x)
    val odd  = nums.head.toLong - (if nums.head % 2 == 0 then x else 0)

    nums.tail
      .foldLeft(even, odd) { case ((even, odd), n) =>
        if n % 2 == 0 then (n + even.max(odd - x), odd)
        else (even, n + odd.max(even - x))
      }
      .pipe { case (even, odd) => even.max(odd) }
