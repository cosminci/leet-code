package io.github.cosminci.leetcode._500

object _458_PoorPigs:

  def poorPigs(buckets: Int, minutesToDie: Int, minutesToTest: Int): Int =
    Iterator
      .iterate(0)(_ + 1)
      .dropWhile(pigs => math.pow(minutesToTest / minutesToDie + 1, pigs) < buckets)
      .next()

  def poorPigs2(buckets: Int, minutesToDie: Int, minutesToTest: Int): Int =
    math.ceil(math.log(buckets) / math.log(minutesToTest / minutesToDie + 1)).toInt
