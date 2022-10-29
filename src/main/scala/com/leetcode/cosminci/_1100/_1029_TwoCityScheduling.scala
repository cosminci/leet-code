package com.leetcode.cosminci._1100

object _1029_TwoCityScheduling:
  
  def twoCitySchedCost(costs: Array[Array[Int]]): Int =
    val (bestA, bestB) = costs.sortBy(c => c.last - c.head).splitAt(costs.length / 2)
    bestA.map(_.head).sum + bestB.map(_.last).sum
