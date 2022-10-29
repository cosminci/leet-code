package com.leetcode.cosminci._2100

import scala.collection.mutable

object _2071_MaxNumberOfTasksYouCanAssign:
  def main(args: Array[String]): Unit =
    println(maxTaskAssign(Array(3, 2, 1), Array(0, 3, 3), 1, 1))

  def maxTaskAssign(tasks: Array[Int], workers: Array[Int], pills: Int, pStr: Int): Int =
    tasks.sortInPlace()
    workers.sortInPlace()

    def executeTasks(taskCount: Int): Boolean =
      var pillsLeft = pills
      val workforce = mutable.TreeMap.from(workers.takeRight(taskCount).groupBy(identity).view.mapValues(_.length))
      tasks.take(taskCount).reverse.foreach { tStr =>
        val (wStr, count) = workforce.last
        if tStr <= wStr then
          if count == 1 then workforce.remove(wStr) else workforce.update(wStr, count - 1)
        else if tStr <= workforce.last._1 + pStr && pillsLeft > 0 then
          workforce.minAfter(tStr - pStr - 1).foreach { case (wStr, count) =>
            if count == 1 then workforce.remove(wStr) else workforce.update(wStr, count - 1)
          }
          pillsLeft -= 1
        else return false
      }
      true

    var (lo, hi) = (0, tasks.length.min(workers.length))
    while lo < hi do
      val mid = lo + hi + 1 >> 1
      if executeTasks(mid) then lo = mid
      else hi = mid - 1
    lo
