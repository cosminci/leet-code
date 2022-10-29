package com.leetcode.cosminci._300

object _287_FindTheDuplicateNumber:

  def main(args: Array[String]): Unit =
    println(findDuplicate(Array(1, 3, 4, 2, 2)))
    println(findDuplicate(Array(3, 1, 3, 4, 2)))
    println(findDuplicate(Array(1, 1)))
    println(findDuplicate(Array(1, 1, 2)))
    println(findDuplicate(Array(2, 5, 9, 6, 9, 3, 8, 9, 7, 1)))

  def findDuplicate(nums: Array[Int]): Int =
    @annotation.tailrec
    def intersectionPoint(slow: Int, fast: Int): Int =
      if (nums(slow) == nums(fast)) slow
      else intersectionPoint(nums(slow), nums(nums(fast)))

    @annotation.tailrec
    def cycleStart(a: Int, b: Int): Int =
      if (nums(a) == nums(b)) nums(a)
      else cycleStart(nums(a), nums(b))

    cycleStart(a = 0, b = intersectionPoint(slow = nums(0), fast = nums(nums(0))))
