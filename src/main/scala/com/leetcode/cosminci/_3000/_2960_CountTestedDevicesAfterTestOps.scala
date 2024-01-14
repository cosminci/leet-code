package com.leetcode.cosminci._3000

object _2960_CountTestedDevicesAfterTestOps:

  def countTestedDevices(batteryPercentages: Array[Int]): Int =
    batteryPercentages.foldLeft(0) { (tested, battery) =>
      if battery - tested > 0 then tested + 1 else tested
    }
