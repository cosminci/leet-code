package com.leetcode.cosminci._3000

import scala.util.chaining.*

object _3000_MaxAreaOfLongestDiagonalRectangle:

  def areaOfMaxDiagonal(dimensions: Array[Array[Int]]): Int =
    dimensions
      .maxBy { case Array(x, y) => (math.sqrt(x.toLong * x + y.toLong * y), x * y) }
      .pipe { case Array(x, y) => x * y }
