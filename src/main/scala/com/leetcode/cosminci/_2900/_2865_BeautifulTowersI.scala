package com.leetcode.cosminci._2900

object _2865_BeautifulTowersI:

  def maximumSumOfHeights(maxHeights: List[Int]): Long =
    def computeSum(range: Range, start: Int) =
      range
        .foldLeft(0L, start) { case ((sum, min), j) =>
          val newMin = min.min(maxHeights(j))
          (sum + newMin, newMin)
        }._1

    maxHeights.indices.map { i =>
      val leftHeightsSum  = computeSum(i - 1 to 0 by -1, maxHeights(i))
      val rightHeightsSum = computeSum(i + 1 until maxHeights.length, maxHeights(i))
      leftHeightsSum + maxHeights(i) + rightHeightsSum
    }.max
