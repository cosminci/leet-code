package io.github.cosminci.leetcode._600

import scala.collection.mutable

object _503_NextGreaterElementII:
  def main(args: Array[String]): Unit =
    println(nextGreaterElements(Array(5, 4, 3, 2, 1)).toSeq)
    println(nextGreaterElements(Array(1, 2, 3, 4, 3)).toSeq)

  private def nextGreaterElements(nums: Array[Int]): Array[Int] =
    val stack  = mutable.Stack.from(nums)
    val result = Array.fill(nums.length)(-1)

    nums.indices.reverse.foreach { i =>
      stack.popWhile(_ <= nums(i))
      stack.headOption.foreach(greater => result.update(i, greater))
      stack.push(nums(i))
    }

    result
