package com.leetcode.cosminci._2800

object _2739_TotalDistanceTraveled:

  def distanceTraveled(mainTank: Int, additionalTank: Int): Int =
    (mainTank + ((mainTank - 1) / 4).min(additionalTank)) * 10
