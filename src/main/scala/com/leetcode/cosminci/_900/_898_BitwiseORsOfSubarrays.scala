package com.leetcode.cosminci._900

object _898_BitwiseORsOfSubarrays {
  def subarrayBitwiseORs(arr: Array[Int]): Int =
    arr.scanLeft(Array.empty[Int]) { (prevBitsets, n) =>
      (prevBitsets :+ 0).map(_ | n).distinct
    }.flatten.distinct.length
}
