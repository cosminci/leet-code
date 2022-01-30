package io.github.cosminci.leetcode._2200

object _2154_KeepMultiplyingFoundValuesByTwo {

  def findFinalValue(nums: Array[Int], original: Int): Int =
    Iterator
      .iterate(original)(n => n * 2)
      .dropWhile(nums.toSet.contains)
      .next()
}
