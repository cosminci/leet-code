package com.leetcode.cosminci._1000

import scala.collection.mutable

object _907_SumOfSubarraytMins:
  def main(args: Array[String]): Unit =
    println(sumSubarrayMins(Array(3, 1, 2, 4)))

  def sumSubarrayMins(arr: Array[Int]): Int =
    val nums   = 0 +: arr
    val result = Array.fill(nums.length)(0L)
    val stack  = mutable.Stack(0)

    (0 until nums.length).foreach { i =>
      stack.popWhile(j => nums(j) > nums(i))
      val j = stack.head
      result(i) = result(j) + (i - j) * nums(i)
      stack.push(i)
    }

    (result.sum % 1_000_000_007).toInt
