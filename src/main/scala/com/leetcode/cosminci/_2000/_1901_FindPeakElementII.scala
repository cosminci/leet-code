package com.leetcode.cosminci._2000

object _1901_FindPeakElementII:
  def main(args: Array[String]): Unit =
    println(
      findPeakGrid(
        Array(
          Array(7, 2, 3, 1, 2),
          Array(6, 5, 4, 2, 1)
        )
      ).toList
    )

  def findPeakGrid(mat: Array[Array[Int]]): Array[Int] =
    var (top, bottom) = (0, mat.length - 1)

    while top < bottom do
      val mid = top + (bottom - top) / 2
      if mat(mid).max > mat(mid + 1).max then bottom = mid
      else top = mid + 1

    Array(bottom, mat(bottom).indexOf(mat(bottom).max))
