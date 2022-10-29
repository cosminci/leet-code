package com.leetcode.cosminci._2400

object _2391_MinTimeToCollectGarbage:

  def garbageCollection(garbage: Array[String], travel: Array[Int]): Int =
    def collectTime(kind: Char) = garbage.map(_.count(_ == kind)).sum

    val travelPrefixSum = travel.scanLeft(0)(_ + _)
    def travelTime(kind: Char) =
      garbage.indices
        .findLast(i => garbage(i).contains(kind))
        .map(travelPrefixSum)
        .getOrElse(0)

    "MPG".map(kind => travelTime(kind) + collectTime(kind)).sum
