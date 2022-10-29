package com.leetcode.cosminci._500

object _448_FindAllNumbersDissapeared:
  def main(args: Array[String]): Unit =
    println(findDisappearedNumbers(Array(4, 3, 2, 7, 8, 2, 3, 1)))

  def findDisappearedNumbers(nums: Array[Int]): List[Int] =
    nums.indices.foreach { i =>
      val foundNumberIdx = math.abs(nums(i)) - 1
      if nums(foundNumberIdx) > 0 then nums(foundNumberIdx) = -nums(foundNumberIdx)
    }
    nums.indices.collect { case i if nums(i) > 0 => i + 1 }.toList
