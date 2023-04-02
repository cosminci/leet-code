package com.leetcode.cosminci._2700

object _2606_FindSubstringWithMaxCost:

  def maximumCostSubstring(s: String, chars: String, vals: Array[Int]): Int =
    val charCosts = chars.zip(vals).toMap
    s.foldLeft(0, 0) { case ((maxCost, runningCost), ch) =>
      val newRunningCost = (runningCost + charCosts.getOrElse(ch, ch - 'a' + 1)).max(0)
      (maxCost.max(newRunningCost), newRunningCost)
    }._1
