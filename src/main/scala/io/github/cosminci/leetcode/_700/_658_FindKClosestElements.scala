package io.github.cosminci.leetcode._700

import scala.annotation.tailrec

object _658_FindKClosestElements:

  def main(args: Array[String]): Unit =
    println(findClosestElements(Array(0, 0, 0, 1, 3, 5, 6, 7, 8, 8), 2, 2))

  def findClosestElements(arr: Array[Int], k: Int, x: Int): List[Int] =
    var (l, r) = (0, arr.length - 1)

    while r - l >= k do
      if math.abs(x - arr(r)) >= math.abs(x - arr(l)) then r -= 1
      else l += 1

    arr.slice(l, r + 1).toList
