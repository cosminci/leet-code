package io.github.cosminci.leetcode._200

object _136_SingleNumber:
  def main(args: Array[String]): Unit =
    println(singleNumber(Array(4, 1, 2, 1, 2)))

  def singleNumber(nums: Array[Int]): Int =
    nums.tail.foldLeft(nums.head) { case (acc, n) =>
      acc ^ n
    }
