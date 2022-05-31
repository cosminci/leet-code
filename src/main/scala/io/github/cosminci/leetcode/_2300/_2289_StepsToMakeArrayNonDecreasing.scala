package io.github.cosminci.leetcode._2300

object _2289_StepsToMakeArrayNonDecreasing:

  def totalSteps(nums: Array[Int]): Int =
    nums.tail
      .foldLeft(Array((nums.head, 0)), 0) { case ((stack, maxSteps), n) =>
        val (newStack, steps) = Iterator
          .iterate((stack, 0)) { case (stack, steps) => (stack.dropRight(1), steps.max(stack.last._2)) }
          .dropWhile(_._1.lastOption.exists(_._1 <= n))
          .next()
        val localSteps = if newStack.nonEmpty then steps + 1 else 0
        (newStack :+ (n, localSteps), maxSteps.max(localSteps))
      }._2
