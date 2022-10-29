package com.leetcode.cosminci._1300


object _1235_MaxProfitInJobScheduling {
  def main(args: Array[String]): Unit = {
    println(jobScheduling(Array(4, 2, 4, 8, 2), Array(5, 5, 5, 10, 8), Array(1, 2, 8, 10, 4)))
    println(jobScheduling(Array(1, 2, 3, 4, 6), Array(3, 5, 10, 6, 9), Array(20, 20, 100, 70, 60)))
    println(jobScheduling(Array(1, 1, 1), Array(2, 3, 4), Array(5, 6, 4)))
  }

  case class Job(start: Int, end: Int, profit: Int)
  def jobScheduling(startTime: Array[Int], endTime: Array[Int], profit: Array[Int]): Int = {
    val jobs = startTime.zip(endTime).zip(profit).map {
      case ((s, e), p) => Job(s, e, p)
    }.sortBy(_.end)

    def latestNonConflicting(endIdx: Int, targetStart: Int): Int = {
      var (l, r) = (0, endIdx)
      while (l < r) {
        val mid = l + (r - l) / 2
        if (jobs(mid).end <= targetStart)
          l = mid + 1
        else
          r = mid
      }
      l - 1
    }

    val dp = Array.ofDim[Int](jobs.length)
    dp(0) = jobs.head.profit

    (1 until jobs.length).foreach { i =>
      val latestAv = latestNonConflicting(i, jobs(i).start)
      val profit = jobs(i).profit + (if (latestAv != -1) dp(latestAv) else 0)
      dp(i) = math.max(profit, dp(i - 1))
    }

    dp.last
  }
}
