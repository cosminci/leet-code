package io.github.cosminci.leetcode._700

object _665_NonDecreasingArray:
  def main(args: Array[String]): Unit =
    println(checkPossibility(Array(-1, 4, 2, 3)))

  def checkPossibility(nums: Array[Int]): Boolean =
    val deltas  = nums.indices.tail.map(i => nums(i) - nums(i - 1))
    deltas.zipWithIndex.filter(_._1 < 0) match
      case Seq() => true
      case Seq((negative, idx)) =>
        idx == 0 || idx == deltas.length - 1 || negative + deltas(idx + 1) >= 0 || deltas(idx - 1) + negative >= 0
      case _ => false
