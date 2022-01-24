package io.github.cosminci.leetcode._2200

object _2149_RearrangeArrayElementsBySign:

  def rearrangeArray(nums: Array[Int]): Array[Int] =
    val (positive, negative) = nums.partition(_ > 0)
    positive.zip(negative).flatMap { case (a, b) => Array(a, b) }
