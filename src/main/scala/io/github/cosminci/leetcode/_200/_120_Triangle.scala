package io.github.cosminci.leetcode._200

object _120_Triangle:

  def minimumTotal(triangle: List[List[Int]]): Int =
    triangle
      .foldRight(Seq.fill(triangle.last.length + 1)(0)) { (row, dp) =>
        row.indices.map(i => row(i) + dp(i).min(dp(i + 1)))
      }
      .head
