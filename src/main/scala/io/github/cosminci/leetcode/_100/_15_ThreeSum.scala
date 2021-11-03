package io.github.cosminci.leetcode._100

import scala.collection.Searching.*
import scala.language.implicitConversions

object _15_ThreeSum:
  def main(args: Array[String]): Unit =
    println(threeSum(Array(-1, 0, 1, 2, -1, -4)))

  def threeSum(nums: Array[Int]): List[List[Int]] =
    nums.sortInPlace()

    given Conversion[SearchResult, Option[Int]] =
      case Found(k) => Some(k)
      case _        => None

    val results = for
      i <- 0 until nums.length - 2 if (i == 0 || nums(i - 1) != nums(i))
      j <- i + 1 until nums.length - 1 if (j == i + 1 || nums(j - 1) != nums(j))
      k <- nums.search(-(nums(i) + nums(j)), j + 1, nums.length)
    yield List(nums(i), nums(j), nums(k))

    results.toList
