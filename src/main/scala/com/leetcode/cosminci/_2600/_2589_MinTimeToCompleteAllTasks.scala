package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2589_MinTimeToCompleteAllTasks:

  def findMinimumTime(tasks: Array[Array[Int]]): Int =
    tasks
      .sortBy { case Array(_, end, _) => end }
      .foldLeft(Set.empty[Int]) { case (chosen, Array(start, end, duration)) =>
        val minimized = chosen.foldLeft(duration)((d, t) => if t >= start && t <= end then d - 1 else d)
        Iterator
          .iterate((minimized, end, chosen)) { case (d, end, chosen) =>
            if chosen.contains(end) then (d, end - 1, chosen) else (d - 1, end - 1, chosen + end)
          }
          .dropWhile { case (duration, _, _) => duration > 0 }.next()
          .pipe { case (_, _, chosen) => chosen }
      }
      .pipe { chosen => chosen.size }
