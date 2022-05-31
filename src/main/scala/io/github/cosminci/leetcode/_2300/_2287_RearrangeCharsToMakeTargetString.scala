package io.github.cosminci.leetcode._2300

object _2287_RearrangeCharsToMakeTargetString:

  def rearrangeCharacters(s: String, target: String): Int =
    val budget = s.groupMapReduce(identity)(_ => 1)(_ + _)
    target
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .map { case (char, need) => budget.getOrElse(char, 0) / need }
      .min
