package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _179_LargestNumber:

  def main(args: Array[String]): Unit =
    println(largestNumber(Array(34323, 3432)))
    println(largestNumber(Array(3, 30, 34, 5, 9)))

  def largestNumber(nums: Array[Int]): String =
    if nums.toSet == Set(0) then return "0"

    given Ordering[Int] = (x: Int, y: Int) => s"$y$x".compare(s"$x$y")

    nums.sorted.mkString
