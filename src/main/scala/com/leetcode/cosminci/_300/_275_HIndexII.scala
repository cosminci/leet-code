package com.leetcode.cosminci._300

object _275_HIndexII:
  def main(args: Array[String]): Unit =
    println(hIndex(Array(0, 1, 3, 5, 6)))
    println(hIndex(Array(1, 2, 100)))
    println(hIndex(Array(5, 5, 5, 6, 7)))
    println(hIndex(Array(0)))

  def hIndex(citations: Array[Int]): Int =
    val n      = citations.length
    var (l, r) = (0, n - 1)
    while l <= r do
      val mid = l + (r - l) / 2
      if citations(mid) < n - mid then l = mid + 1
      else r = mid - 1
    n - l
