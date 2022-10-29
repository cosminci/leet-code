package com.leetcode.cosminci._500

object _477_TotalHammingDistance:
  def main(args: Array[String]): Unit =
    println(totalHammingDistance(Array(4, 14, 2)))
    println(totalHammingDistance(Array(4, 14, 4)))

  def totalHammingDistance(nums: Array[Int]): Int =
    (0 until 32)
      .map(i => nums.fold(0)((bitCount, n) => bitCount + ((n >> i) & 1)))
      .foldLeft(0)((total, bitCount) => total + bitCount * (nums.length - bitCount))
