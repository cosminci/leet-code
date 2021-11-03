package io.github.cosminci.leetcode._2000

import scala.collection.mutable

object _1986_MinNumberOfWorkSessionsToFinishTheTasks:
  def main(args: Array[String]): Unit =
    println(minSessions(Array(2, 4), 5))
    println(minSessions(Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 14))

  def minSessions(tasks: Array[Int], sessionTime: Int): Int =
    val mem = mutable.Map.empty[(Int, Seq[Int]), Int]

    def dfs(idx: Int, sessions: Seq[Int]): Int =
      if idx == tasks.length then return sessions.length
      if mem.contains((idx, sessions)) then return mem((idx, sessions))

      val result = sessions.indices
        .collect {
          case i if sessions(i) + tasks(idx) <= sessionTime =>
            sessions.updated(i, sessions(i) + tasks(idx))
        }
        .appended(sessions.appended(tasks(idx)))
        .map(dfs(idx + 1, _))
        .min

      mem.update((idx, sessions), result)
      result

    dfs(0, Seq.empty)
