package com.leetcode.cosminci._2300

object _2279_MaxBagsWithFullCapacityOfRocks:

  def maximumBags(capacity: Array[Int], rocks: Array[Int], additionalRocks: Int): Int =
    capacity
      .zip(rocks)
      .map { case (c, r) => c - r }
      .sorted
      .foldLeft(0, additionalRocks) { case ((fullBags, rocksLeft), capacityLeft) =>
        if capacityLeft == 0 then (fullBags + 1, rocksLeft)
        else
          val rocksToUse      = capacityLeft.min(rocksLeft)
          val newRocksLeft    = rocksLeft - rocksToUse
          val newCapacityLeft = capacityLeft - rocksToUse
          if newCapacityLeft == 0 then (fullBags + 1, newRocksLeft)
          else return fullBags
      }
      ._1
