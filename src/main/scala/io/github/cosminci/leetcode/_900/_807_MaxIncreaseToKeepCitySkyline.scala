package io.github.cosminci.leetcode._900

object _807_MaxIncreaseToKeepCitySkyline:
  def main(args: Array[String]): Unit =
    println(maxIncreaseKeepingSkyline(
      Array(
        Array(3, 0, 8, 4),
        Array(2, 4, 5, 7),
        Array(9, 2, 6, 3),
        Array(0, 3, 1, 0)
      )
    ))

  private def maxIncreaseKeepingSkyline(grid: Array[Array[Int]]): Int =
    val rowMax = grid.map(_.max)
    val colMax = grid.transpose.map(_.max)

    val increases = for
      r <- grid.indices
      c <- grid(r).indices
    yield rowMax(r).min(colMax(c)) - grid(r)(c)

    increases.sum
