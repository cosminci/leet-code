package io.github.cosminci.leetcode._2100

object _2008_MaxEarningsFromTaxi:
  def main(args: Array[String]): Unit =
    println(
      maxTaxiEarnings(
        20,
        Array(
          Array(1, 6, 1),
          Array(3, 10, 2),
          Array(10, 12, 3),
          Array(11, 12, 2),
          Array(12, 15, 2),
          Array(13, 18, 1)
        )
      )
    )

  private def maxTaxiEarnings(n: Int, rides: Array[Array[Int]]): Long =
    rides.sortInPlaceBy { case Array(_, end, _) => end }

    val dp      = Array.ofDim[Long](n + 1)
    var rideIdx = 0
    (1 to n).foreach { i =>
      while rideIdx < rides.length && rides(rideIdx)(1) == i do
        val Array(start, end, tip) = rides(rideIdx)
        dp(i) = math.max(dp(end), dp(start) + end - start + tip)
        rideIdx += 1
      dp(i) = math.max(dp(i), dp(i - 1))
    }

    dp.last
