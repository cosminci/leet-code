package io.github.cosminci.leetcode._2100

object _2042_CheckIfNumbersAreAscendingInSentence:
  def main(args: Array[String]): Unit =
    println(areNumbersAscending("1 box has 3 blue 4 red 6 green and 12 yellow marbles"))

  def areNumbersAscending(s: String): Boolean =
    val nums = s.split(" ").flatMap(_.toIntOption)
    (1 until nums.length).forall(i => nums(i) > nums(i - 1))
