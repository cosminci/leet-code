package com.leetcode.cosminci._2400

object _2334_SubarrayWithElementsGreaterThanThreshold:

  def validSubarraySize(nums0: Array[Int], threshold: Int): Int =
    val nums = nums0 :+ 0
    nums.indices.foldLeft(Array.empty[Int]) { (stack, i) =>
      Iterator
        .iterate(stack) { stack =>
          val stack0 = stack.dropRight(1)
          val n      = nums(stack.last)
          val k      = stack0.lastOption.map(j => i - j - 1).getOrElse(i)
          if n > threshold / k then return k
          stack0
        }
        .dropWhile(_.lastOption.exists(j => nums(i) <= nums(j)))
        .next() :+ i
    }
    -1
