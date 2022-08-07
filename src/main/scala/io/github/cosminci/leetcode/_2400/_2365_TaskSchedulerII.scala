package io.github.cosminci.leetcode._2400

object _2365_TaskSchedulerII:

  def taskSchedulerII(tasks: Array[Int], space: Int): Long =
    tasks
      .foldLeft(Map.empty[Int, Long], 0L) { case ((lastStarted, currDay), taskType) =>
        val nextDay = lastStarted.get(taskType) match
          case None => currDay + 1
          case Some(prevDay) =>
            if currDay - prevDay > space then currDay + 1
            else currDay + space - (currDay - prevDay) + 1
        (lastStarted + (taskType -> nextDay), nextDay)
      }
      ._2
