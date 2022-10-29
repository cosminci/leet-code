package com.leetcode.cosminci._2200

object _2144_MinCostOfBuyingCandiesWithDiscount:
  def minimumCost(cost: Array[Int]): Int =
    cost.sortBy(c => -c)
      .grouped(3)
      .flatMap(_.take(2))
      .sum
