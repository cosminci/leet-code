package io.github.cosminci.leetcode._2200

object _2125_NumberOfLaserBeamsInBank {
  def numberOfBeams(bank: Array[String]): Int =
    bank.foldLeft(0, 0) { case ((res, prev), curr) =>
      val devices = curr.count(_ == '1')
      if (devices == 0) (res, prev)
      else (res + prev * devices, devices)
    }._1
}
