package com.leetcode.cosminci._2900

object _2857_CountPairsOfPointsWithDistanceK:

  def countPairs(coordinates: List[List[Int]], k: Int): Int =
    coordinates
      .map(l => (l.head, l.last))
      .foldLeft(Map.empty[(Int, Int), Int].withDefaultValue(0), 0) {
        case ((counter, res), (x, y)) =>
          val newRes     = res + (0 to k).map(i => counter((x ^ i, y ^ (k - i)))).sum
          val newCounter = counter.updated((x, y), counter((x, y)) + 1)
          (newCounter, newRes)
      }._2
