package com.leetcode.cosminci._3100

object _3047_FindLargestAreaOfSquareInsideTwoRectangles:

  def largestSquareArea(bottomLeft: Array[Array[Int]], topRight: Array[Array[Int]]): Long =
    bottomLeft.indices.foldLeft(0L) { (max, i) =>
      (i + 1 until topRight.length).foldLeft(max) { (res, j) =>
        val minX = bottomLeft(i)(0).max(bottomLeft(j)(0))
        val maxX = topRight(i)(0).min(topRight(j)(0))
        val minY = bottomLeft(i)(1).max(bottomLeft(j)(1))
        val maxY = topRight(i)(1).min(topRight(j)(1))
        if minX >= maxX || minY >= maxY then res
        else
          val side = (maxX - minX).min(maxY - minY)
          res.max(side.toLong * side)
      }
    }
