package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _454_FourSumII:
  def main(args: Array[String]): Unit =
    println(fourSumCount(Array(1, 2), Array(-2, -1), Array(-1, 2), Array(0, 2)))

  def fourSumCount(nums1: Array[Int], nums2: Array[Int], nums3: Array[Int], nums4: Array[Int]): Int =
    val nums12Sums = mutable.Map.empty[Int, Int]
    nums1.foreach { n1 =>
      nums2.foreach { n2 =>
        nums12Sums.update(n1 + n2, nums12Sums.getOrElse(n1 + n2, 0) + 1)
      }
    }
    var result = 0
    nums3.foreach { n3 =>
      nums4.foreach { n4 =>
        result += nums12Sums.getOrElse(-(n3 + n4), 0)
      }
    }
    result
