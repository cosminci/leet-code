package com.leetcode.cosminci._2500

object _2469_ConvertTemperature:

  def convertTemperature(celsius: Double): Array[Double] =
    Array(celsius + 273.15, celsius * 1.8 + 32)
