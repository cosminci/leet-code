package com.leetcode.cosminci._2300

object _2210_CountHillsAndValleysInAnArray:
  def countHillValley(nums: Array[Int]): Int =
    nums
      .foldLeft(Array.empty[Int]) { (distinct, n) =>
        if (distinct.isEmpty || n != distinct.last) distinct :+ n else distinct
      }
      .sliding(3)
      .filter(_.length == 3)
      .count { case Array(l, mid, r) =>
        (l < mid && mid > r) || (l > mid && mid < r)
      }
