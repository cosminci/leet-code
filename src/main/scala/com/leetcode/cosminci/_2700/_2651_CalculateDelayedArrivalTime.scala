package com.leetcode.cosminci._2700

object _2651_CalculateDelayedArrivalTime:

  def findDelayedArrivalTime(arrivalTime: Int, delayedTime: Int): Int =
    (arrivalTime + delayedTime) % 24
