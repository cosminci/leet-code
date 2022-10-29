package com.leetcode.cosminci._100

object _74_SearchIn2DMatrix:

  def main(args: Array[String]): Unit =
    println(
      searchMatrix(
        Array(
          Array(1, 3, 5, 7),
          Array(10, 11, 16, 20),
          Array(23, 30, 34, 60)
        ),
        0
      )
    )

  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean =
    def binarySearch(arr: Array[Int], t: Int): Int =
      @annotation.tailrec
      def dfs(l: Int, r: Int): Int =
        if l >= r then (l - 1).max(0)
        else
          val mid = l + (r - l) / 2
          if arr(mid) > t then dfs(l, r = mid)
          else dfs(l = mid + 1, r)
      dfs(l = 0, r = arr.length)

    val targetRow = binarySearch(matrix.map(_.head), target)
    val targetCol = binarySearch(matrix(targetRow), target)

    matrix(targetRow)(targetCol) == target
