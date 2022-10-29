package com.leetcode.cosminci._1000

object _985_SumOfEvenNumsAfterQueries:

  def sumEvenAfterQueries(nums: Array[Int], queries: Array[Array[Int]]): Array[Int] =
    queries
      .scanLeft(nums.filter(_ % 2 == 0).sum) { case (sum, Array(v, i)) =>
        val prev = Option(nums(i)).filter(_ % 2 == 0).getOrElse(0)
        nums(i) += v
        val curr = Option(nums(i)).filter(_ % 2 == 0).getOrElse(0)
        sum - prev + curr
      }
      .tail
