package com.leetcode.cosminci._500

import scala.collection.mutable

object _496_NextGreaterElementI:
  def main(args: Array[String]): Unit =
    println(nextGreaterElement(Array(4, 1, 2), Array(1, 2, 4, 2)).toSeq)

  def nextGreaterElement(nums1: Array[Int], nums2: Array[Int]): Array[Int] =
    val stack = mutable.Stack.empty[Int]
    val next  = mutable.Map.empty[Int, Int].withDefaultValue(-1)

    nums2.foreach { n =>
      while stack.headOption.exists(_ < n) do next.update(stack.pop(), n)
      stack.push(n)
    }

    nums1.map(next)
