package com.leetcode.cosminci._1600

import scala.collection.mutable

object _1547_MinCostToCutAStick:

  def minCost(n: Int, cuts: Array[Int]): Int =
    val cuts1 = 0 +: cuts.sorted :+ n

    val dp = Array.ofDim[Int](cuts1.length, cuts1.length)
    for
      i <- dp.indices.reverse
      j <- i + 1 until dp.length
    do dp(i)(j) = if j - i == 1 then 0 else (i + 1 until j).map(k => dp(i)(k) + dp(k)(j)).min + cuts1(j) - cuts1(i)

    dp(0)(dp.length - 1)
