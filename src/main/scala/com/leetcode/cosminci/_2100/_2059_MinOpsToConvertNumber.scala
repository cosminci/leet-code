package com.leetcode.cosminci._2100

import scala.collection.mutable

object _2059_MinOpsToConvertNumber:
  def minimumOperations(nums: Array[Int], start: Int, goal: Int): Int =
    if start == goal then return 0

    val toVisit = mutable.Queue((start, 0))
    val visited = mutable.Set(start)

    while toVisit.nonEmpty do
      val (curr, ops) = toVisit.dequeue()
      nums.flatMap(n => Array(curr + n, curr - n, curr ^ n)).foreach { next =>
        if next == goal then return ops + 1
        if next >= 0 && next <= 1000 && !visited.contains(next) then
          toVisit.enqueue((next, ops + 1))
          visited.add(next)
      }

    -1
