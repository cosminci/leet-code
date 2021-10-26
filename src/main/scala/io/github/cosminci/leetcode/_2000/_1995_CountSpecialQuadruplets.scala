package io.github.cosminci.leetcode._2000

import scala.collection.mutable

object _1995_CountSpecialQuadruplets:
  def main(args: Array[String]): Unit =
    Seq(
      Array(23, 3, 76, 10, 21, 50, 34, 30, 82, 58, 30, 71, 56, 49, 12, 90, 2, 53),
      Array(28, 8, 49, 85, 37, 90, 20, 8)
    ).foreach { nums =>
      println(countQuadruplets(nums))
      println(countQuadrupletsBruteForce(nums))
    }

  private def countQuadrupletsBruteForce(nums: Array[Int]) =
    nums.indices.combinations(4).count { case Seq(a, b, c, d) => nums(a) + nums(b) + nums(c) == nums(d) }

  private def countQuadruplets(nums: Array[Int]): Int =
    var count = 0

    (0 to nums.length - 4).foreach { a =>
      (a + 1 to nums.length - 3).foreach { b =>
        val seenSums = mutable.Map(nums(a) + nums(b) + nums(b + 1) -> 1)
        (b + 2 to nums.length - 1).foreach { c =>
          count += seenSums.getOrElse(nums(c), 0)
          seenSums.update(nums(a) + nums(b) + nums(c), seenSums.getOrElse(nums(a) + nums(b) + nums(c), 0) + 1)
        }
      }
    }

    count
