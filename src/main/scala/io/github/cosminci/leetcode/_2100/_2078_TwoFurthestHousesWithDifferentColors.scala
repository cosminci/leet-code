package io.github.cosminci.leetcode._2100

object _2078_TwoFurthestHousesWithDifferentColors:
  def maxDistance(colors: Array[Int]): Int =
    colors.zipWithIndex.foldLeft(0) { case (prevMax, (color, idx)) =>
      val distToHead = Option.when(color != colors.head)(prevMax.max(idx)).getOrElse(prevMax)
      val distToLast = Option.when(color != colors.last)(prevMax.max(colors.length - 1 - idx)).getOrElse(prevMax)
      distToHead max distToLast
    }
