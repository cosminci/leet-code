package com.leetcode.cosminci._2800

import com.leetcode.cosminci.utils

import scala.util.chaining.*

object _2747_CountZeroRequestServers:

  def countServers(n: Int, logs: Array[Array[Int]], x: Int, queries: Array[Int]): Array[Int] =
    logs.sortInPlaceBy { case Array(server, time) => (time, server) }

    queries.zipWithIndex.sorted.foldLeft(Map.empty[Int, Int], Map.empty[Int, Int], 0, 0) {
      case ((res, active, i, j), (t, id)) =>
        val (active1, i1) = Iterator.iterate((active, i)) { case (active, i) => 
          (active.updated(logs(i)(0), active.getOrElse(logs(i)(0), 0) + 1), i + 1)
        }.dropWhile { case (_, i) => i < logs.length && logs(i)(1) <= t }.next()

        val (active2, j1) = Iterator.iterate((active1, j)) { case (active, j) => 
          (utils.decrementCounter(active, logs(j)(0)), j + 1)
        }.dropWhile { case (_, j) => j < logs.length && logs(j)(1) < t - x }.next()

        (res.updated(id, n - active2.size), active2, i1, j1)
    }.pipe { case (res, _, _, _) =>
      res.toArray.sortBy { case (i, _) => i }.map { case (i, v) => v }
    }
