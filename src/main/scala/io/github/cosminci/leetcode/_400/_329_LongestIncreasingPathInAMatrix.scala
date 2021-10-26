package io.github.cosminci.leetcode._400

import io.github.cosminci.utils

import scala.collection.mutable

object _329_LongestIncreasingPathInAMatrix:
  def main(args: Array[String]): Unit =
    println(
      longestIncreasingPath(
        Array(
          Array(9, 9, 4),
          Array(6, 6, 8),
          Array(2, 1, 1)
        )
      )
    )

  private def longestIncreasingPath(matrix: Array[Array[Int]]): Int =
    val computed           = Array.ofDim[Int](matrix.length, matrix.head.length)
    var overallLongestPath = 1

    def dfs(x: Int, y: Int): Int =
      val longestPath = utils
        .neighbours(x, y, matrix)
        .collect {
          case (nx, ny) if matrix(nx)(ny) < matrix(x)(y) =>
            if computed(nx)(ny) != 0 then computed(nx)(ny) else dfs(nx, ny)
        }
        .maxOption
        .getOrElse(0) + 1

      computed(x)(y) = longestPath
      longestPath

    matrix.indices.foreach { row =>
      matrix(row).indices.foreach { col =>
        val coordinate = (row, col)
        if computed(row)(col) == 0 then
          val longestPath = dfs(row, col)
          overallLongestPath = math.max(overallLongestPath, longestPath)
      }
    }

    overallLongestPath
