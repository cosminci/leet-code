package io.github.cosminci.leetcode._2500

object _2428_MaxSumOfHourglass:

  def maxSum(grid: Array[Array[Int]]): Int =
    def hourglassSum(x: Int, y: Int) =
      Seq((-1, -1), (-1, 0), (-1, 1), (0, 0), (1, -1), (1, 0), (1, 1))
        .map { case (dx, dy) => grid(x + dx)(y + dy) }
        .sum

    (1 until grid.length - 1).foldLeft(0) { (prevMax, x) =>
      (1 until grid.head.length - 1).foldLeft(prevMax) { (prevMax, y) =>
        prevMax.max(hourglassSum(x, y))
      }
    }
