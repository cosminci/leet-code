package io.github.cosminci.leetcode._1400

import scala.collection.mutable

object _1335_MinDifficultyOfJobSchedule:

  def minDifficulty(jobDifficulty: Array[Int], d: Int): Int =
    val numJobs = jobDifficulty.length

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(job: Int, daysLeft: Int): Int = mem.getOrElseUpdate((job, daysLeft),
      if daysLeft == 1 then jobDifficulty.slice(job, numJobs).max
      else
        (job to numJobs - daysLeft)
          .foldLeft(Int.MaxValue, 0) { case ((minDiff, prevMaxDiff), lastJobForCurrDay) =>
            val maxDiff = prevMaxDiff.max(jobDifficulty(lastJobForCurrDay))
            (minDiff.min(maxDiff + dfs(lastJobForCurrDay + 1, daysLeft - 1)), maxDiff)
          }._1
    )

    if numJobs < d then -1 else dfs(job = 0, d)
