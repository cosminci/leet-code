package com.leetcode.cosminci.misc

import scala.collection.mutable

object MinNumberOfDecreasingSubsequences:
  def main(args: Array[String]): Unit =
    println(minSubsequences(Array(5, 2, 4, 3, 1, 6)))
    println(minSubsequences(Array(2, 9, 12, 13, 4, 7, 6, 5, 10)))
    println(minSubsequences(Array(1, 1, 1)))

  def minSubsequences(nums: Array[Int]): Int =
    def enough(maxSubseqs: Int): Boolean =
      val stacks = (1 to maxSubseqs).map(_ => mutable.Stack.empty[Int])
      nums.foreach { n =>
        stacks.filter(_.headOption.forall(_ > n)).minByOption(_.headOption.getOrElse(Int.MaxValue) - n) match
          case None =>
            return false
          case Some(stack) =>
            stack.push(n)
      }
      true

    var (l, r) = (1, nums.length)
    while l < r do
      val mid = l + (r - l) / 2
      if enough(mid) then r = mid
      else l = mid + 1
    l
