package io.github.cosminci.leetcode._1300

object _1217_MinCostToMoveChipsToSamePosition:
  def minCostToMoveChips(position: Array[Int]): Int =
    val evenChips = position.count(_ % 2 == 0)
    evenChips.min(position.length - evenChips)
