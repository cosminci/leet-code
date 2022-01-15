package io.github.cosminci.leetcode._2200

object _2132_StampingTheGrid {
  def main(args: Array[String]): Unit = {

    println(possibleToStamp(Array(
      Array(0,0,0,0,0),
      Array(0,0,0,0,0),
      Array(0,0,1,0,0),
      Array(0,0,0,0,1),
      Array(0,0,0,1,1)
    ), 2, 2))
  }

  def possibleToStamp(grid: Array[Array[Int]], stampHeight: Int, stampWidth: Int): Boolean = {
    val (m, n) = (grid.length, grid.head.length)

    def indices() = for {
      r <- 0 until m
      c <- 0 until n
    } yield (r, c)

    def acc(grid: Array[Array[Int]]) = {
      val dp = Array.fill(m + 1, n + 1)(0)
      indices().foreach { case (r, c) =>
        dp(r + 1)(c + 1) = dp(r + 1)(c) + dp(r)(c + 1) - dp(r)(c) + grid(r)(c)
      }
      dp
    }

    def sum(arr: Array[Array[Int]], r1: Int, c1: Int, r2: Int, c2: Int) =
      arr(r2 + 1)(c2 + 1) - arr(r1)(c2 + 1) - arr(r2 + 1)(c1) + arr(r1)(c1)

    val dp = acc(grid)
    val diff = Array.fill(m + 1, n + 1)(0)
    for {
      c <- 0 until n - stampWidth + 1
      r <- 0 until m - stampHeight + 1
      if sum(dp, r, c, r + stampHeight - 1, c + stampWidth - 1) == 0
    } {
      diff(r)(c) += 1
      diff(r)(c + stampWidth) -= 1
      diff(r + stampHeight)(c) -= 1
      diff(r + stampHeight)(c + stampWidth) += 1
    }

    val dp2 = acc(diff)
    indices().forall { case (r, c) =>
      grid(r)(c) == 1 || dp2(r + 1)(c + 1) != 0
    }
  }
}
