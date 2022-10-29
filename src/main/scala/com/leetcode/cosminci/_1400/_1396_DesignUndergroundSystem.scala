package com.leetcode.cosminci._1400

import scala.collection.mutable

object _1396_DesignUndergroundSystem:

  class UndergroundSystem:
    private val ongoing   = mutable.Map.empty[Int, (String, Int)]
    private val durations = mutable.Map.empty[(String, String), mutable.ListBuffer[Int]]

    def checkIn(id: Int, startStation: String, startTs: Int): Unit =
      ongoing.update(id, (startStation, startTs))

    def checkOut(id: Int, endStation: String, endTs: Int): Unit =
      val Some((startStation, startTs)) = ongoing.remove(id)
      durations.getOrElseUpdate((startStation, endStation), mutable.ListBuffer.empty).append(endTs - startTs)

    def getAverageTime(startStation: String, endStation: String): Double =
      durations((startStation, endStation)).sum.toDouble / durations((startStation, endStation)).length
