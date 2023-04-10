package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2615_SumOfDistances:

  def distance(nums: Array[Int]): Array[Long] =
    def fn(group: Seq[Long]): Seq[(Int, Long)] =
      val prefixSum  = group.scanLeft(0L)(_ + _).tail
      val postfixSum = group.scanRight(0L)(_ + _)
      group.zipWithIndex.map { case (index, i) =>
        index.toInt -> (index * i - prefixSum(i) + postfixSum(i) - index * (group.length - i - 1))
      }

    nums.indices
      .groupMap(nums.apply)(_.toLong).values
      .flatMap(fn).toMap
      .pipe(result => Array.tabulate(nums.length)(result.apply))
