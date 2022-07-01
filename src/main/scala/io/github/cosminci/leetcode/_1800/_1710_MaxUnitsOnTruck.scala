package io.github.cosminci.leetcode._1800

object _1710_MaxUnitsOnTruck:

  def maximumUnits(boxTypes: Array[Array[Int]], truckSize: Int): Int =
    boxTypes
      .sortBy { case Array(_, units) => -units }
      .foldLeft(truckSize, 0) { case ((spaceLeft, unitsLoaded), Array(count, units)) =>
        val boxesToUse = spaceLeft.min(count)
        val unitsToUse = boxesToUse * units
        (spaceLeft - boxesToUse, unitsLoaded + unitsToUse)
      }._2
