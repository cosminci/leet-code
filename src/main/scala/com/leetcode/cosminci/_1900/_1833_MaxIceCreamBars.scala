package com.leetcode.cosminci._1900

object _1833_MaxIceCreamBars:

  def maxIceCream(costs: Array[Int], coins: Int): Int =
    costs.sorted.foldLeft(0, 0) { case ((total, count), cost) =>
      if total + cost > coins then (total, count)
      else (total + cost, count + 1)
    }._2
