package com.leetcode.cosminci._500

import scala.collection.mutable

object _456_132_Pattern:
  def main(args: Array[String]): Unit =
    println(find132pattern(Array(1, 0, 1, -4, -3)))
    println(find132pattern(Array(1, 2, 3, 4)))
    println(find132pattern(Array(3, 1, 4, 2)))
    println(find132pattern(Array(-1, 3, 2, 0)))

  def find132pattern(nums: Array[Int]): Boolean =
    val maxStack = mutable.Stack.empty[Int]
    var s3       = Int.MinValue
    nums.reverse.foreach { n =>
      if n < s3 then return true
      while maxStack.headOption.exists(_ < n) do s3 = maxStack.pop()
      maxStack.push(n)
    }
    false
