package io.github.cosminci.leetcode._1500

object _1411_NumberOfWaysToPaintNx3Grid:
  def main(args: Array[String]): Unit =
    println(numOfWaysMathDP(7))
    println(numOfWaysMemoryDP(7))

  def numOfWaysMathDP(n: Int): Int =
    val mod = 1_000_000_007

    var (twoColorCombiCount, threeColorCombiCount) = (6L, 6L)
    (2 to n).foreach { _ =>
      val prevThreeColorCombiCount = threeColorCombiCount
      threeColorCombiCount = (2 * threeColorCombiCount + 2 * twoColorCombiCount)   % mod
      twoColorCombiCount = (2 * prevThreeColorCombiCount + 3 * twoColorCombiCount) % mod
    }

    ((twoColorCombiCount + threeColorCombiCount) % mod).toInt

  def numOfWaysMemoryDP(n: Int): Int =
    val mod    = 1_000_000_007
    val colors = Seq(1, 2, 3)
    val dp     = Array.ofDim[Long](n + 1, 4, 4, 4)

    def dfs(row: Int, prevA: Int, prevB: Int, prevC: Int): Long =
      if row == 0 then return 1
      if dp(row)(prevA)(prevB)(prevC) != 0 then return dp(row)(prevA)(prevB)(prevC)

      dp(row)(prevA)(prevB)(prevC) = (for
        currA <- colors.filter(c => c != prevA)
        currB <- colors.filter(c => c != currA && c != prevB)
        currC <- colors.filter(c => c != currB && c != prevC)
      yield dfs(row - 1, currA, currB, currC)).sum % mod

      dp(row)(prevA)(prevB)(prevC) % mod

    (dfs(n, 0, 0, 0) % mod).toInt
