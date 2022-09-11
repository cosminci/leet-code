package io.github.cosminci.leetcode._2500

object _2404_MostFrequentEvenElement:

  def mostFrequentEven(nums: Array[Int]): Int =
    nums
      .filter(_ % 2 == 0)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .maxByOption { case (num, freq) => (freq, -num) }
      .map { case (num, _) => num }
      .getOrElse(-1)
