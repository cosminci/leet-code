package com.leetcode.cosminci._2300

object _2251_NumFlowersInFullBloom:

  def fullBloomFlowers(flowers: Array[Array[Int]], persons: Array[Int]): Array[Int] =
    val prefixSum = flowers
      .flatMap { case Array(start, end) => Array((start, 1), (end + 1, -1)) }
      .sorted
      .scanLeft((0, 0)) { case ((_, flowersInBloom), (time, delta)) => (time, flowersInBloom + delta) }

    persons.map { time =>
      val idx = prefixSum.search((time, Int.MaxValue)).insertionPoint - 1
      prefixSum(idx)._2
    }
