package io.github.cosminci.leetcode._800

object _740_DeleteAndEarn {
  def main(args: Array[String]): Unit = {
    println(deleteAndEarn(Array(3, 4, 2)))
    println(deleteAndEarn(Array(2, 2, 3, 3, 3, 4)))
  }

  def deleteAndEarn(nums: Array[Int]): Int = {
    val counter = nums.groupMapReduce(identity)(_ => 1)(_ + _)
    (1 to nums.max).foldLeft(0, 0) {
      case ((prev, curr), n) =>
        (curr, math.max(prev + n * counter.getOrElse(n, 0), curr))
    }._2
  }
}
