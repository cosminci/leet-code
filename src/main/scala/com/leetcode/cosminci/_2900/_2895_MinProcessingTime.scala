package com.leetcode.cosminci._2900

object _2895_MinProcessingTime:

  def minProcessingTime(processorTime: List[Int], tasks: List[Int]): Int =
    tasks.sorted.reverse.zipWithIndex
      .collect { case (v, i) if i % 4 == 0 => v }
      .zip(processorTime.sorted)
      .map { case (a, b) => a + b }
      .max
