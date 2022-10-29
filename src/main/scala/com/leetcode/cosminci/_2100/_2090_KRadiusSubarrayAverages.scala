package com.leetcode.cosminci._2100

object _2090_KRadiusSubarrayAverages:
  def main(args: Array[String]): Unit =
    println(getAverages(Array(7, 4, 3, 9, 1, 8, 5, 2, 6), 3).toSeq)

  def getAverages(nums: Array[Int], k: Int): Array[Int] =
    if nums.length <= k * 2 then Array.fill(nums.length)(-1)
    else
      val (w, len) = (2 * k + 1, nums.length)
      val averages = (w until len).scanLeft(nums.take(w).map(_.toLong).sum)((sum, i) => sum + nums(i) - nums(i - w))
      Array.fill(k)(-1) ++ averages.map(avg => (avg / w).toInt) ++ Array.fill(k)(-1)
