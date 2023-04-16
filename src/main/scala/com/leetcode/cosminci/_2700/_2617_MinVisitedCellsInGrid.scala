package com.leetcode.cosminci._2700

import scala.collection.mutable

object _2617_MinVisitedCellsInGrid:

  def minimumVisitedCells(grid: Array[Array[Int]]): Int =
    val (m, n) = (grid.length, grid.head.length)
    val rows   = Array.tabulate(m)(_ => mutable.TreeSet.from(0 until n))
    val cols   = Array.tabulate(n)(_ => mutable.TreeSet.from(0 until m))

    val toVisit = mutable.Queue((0, 0, 1))
    while toVisit.nonEmpty do
      val (i, j, steps) = toVisit.dequeue()
      if i == m - 1 && j == n - 1 then return steps

      val reachable1 = rows(i).range(j + 1, (grid(i)(j) + j + 1).min(n))
      val reachable2 = cols(j).range(i + 1, (grid(i)(j) + i + 1).min(m))
      toVisit.enqueueAll(reachable1.map(k => (i, k, steps + 1)))
      toVisit.enqueueAll(reachable2.map(k => (k, j, steps + 1)))
      reachable1.foreach(k => rows(i).remove(k))
      reachable2.foreach(k => cols(j).remove(k))

    -1
