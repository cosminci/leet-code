package com.leetcode.cosminci._2500

import scala.util.chaining.*

object _2455_AvgValOfEvenNumsDivisibleByThree:

  def averageValue(nums: Array[Int]): Int =
    nums
      .filter(_ % 6 == 0)
      .pipe(divisible => if divisible.length == 0 then 0 else divisible.sum / divisible.length)
