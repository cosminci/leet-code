package com.leetcode.cosminci._2800

object _2780_MinIndexOfValidSplit:

  def minimumIndex(nums: List[Int]): Int =
    val (x, f) = nums.groupMapReduce(identity)(_ => 1)(_ + _).maxBy { case (x, f) => f }

    val n = nums.length
    nums.view
      .scanLeft(0)((cnt, n) => cnt + (if n == x then 1 else 0)).tail
      .zipWithIndex
      .collectFirst { case (cnt, i) if cnt * 2 > (i + 1) && (f - cnt) * 2 > (n - i - 1) => i }
      .getOrElse(-1)
