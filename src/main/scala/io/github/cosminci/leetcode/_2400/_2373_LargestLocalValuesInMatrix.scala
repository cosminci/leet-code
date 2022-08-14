package io.github.cosminci.leetcode._2400

object _2373_LargestLocalValuesInMatrix:

  def largestLocal(grid: Array[Array[Int]]): Array[Array[Int]] =
    def max(matrix: Array[Array[Int]]) = matrix.map(_.max).max
    grid.sliding(3).map(_.transpose.sliding(3).map(max).toArray).toArray
