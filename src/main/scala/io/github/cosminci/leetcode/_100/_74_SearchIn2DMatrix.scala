package io.github.cosminci.leetcode._100

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

  private def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean =
    def binarySearch(arr: Array[Int], t: Int): Int =
      var (l, r) = (0, arr.length)
      while l < r do
        val mid = l + (r - l) / 2
        if arr(mid) > t then r = mid
        else l = mid + 1
      math.max(0, l - 1)
    val targetRow = binarySearch(matrix.map(_.head), target)
    val targetCol = binarySearch(matrix(targetRow), target)

    matrix(targetRow)(targetCol) == target
