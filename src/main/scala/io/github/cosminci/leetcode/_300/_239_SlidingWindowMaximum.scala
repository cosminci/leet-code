package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _239_SlidingWindowMaximum:
  def main(args: Array[String]): Unit =
    println(maxSlidingWindow(Array(2, 4, 7), 2).toList)
    println(maxSlidingWindow(Array(7, 2, 4), 2).toList)
    println(maxSlidingWindow(Array(1, -1), 1).toList)
    println(maxSlidingWindow(Array(1, 3, 1, 2, 0, 5), 3).toList)
    println(maxSlidingWindow(Array(1, 3, -1, -3, 5, 3, 6, 7), 3).toList)

  private def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] =
    if k >= nums.length then return Array(nums.max)

    val result = Array.ofDim[Int](nums.length - k + 1)
    val window = new java.util.LinkedList[Int]()

    nums.indices.foreach { i =>
      if !window.isEmpty && window.peek() == i - k then window.pollFirst()
      while !window.isEmpty && nums(i) >= nums(window.peekLast()) do window.pollLast()
      window.offer(i)
      if i - k + 1 >= 0 then result(i - k + 1) = nums(window.peek())
    }

    result
