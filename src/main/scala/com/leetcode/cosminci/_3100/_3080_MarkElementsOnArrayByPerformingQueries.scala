package com.leetcode.cosminci._3100

import scala.collection.mutable
import scala.util.chaining.*

object _3080_MarkElementsOnArrayByPerformingQueries:

  def unmarkedSumArray(nums: Array[Int], queries: Array[Array[Int]]): Array[Long] =
    val pqueue = mutable.PriorityQueue.from(nums.zipWithIndex)(Ordering.by { case (v, i) => (-v, -i) })
    val total  = nums.foldLeft(0L)(_ + _)
    queries
      .foldLeft(Array.empty[Long], Set.empty[Int], total) { case ((ans, marked, total), Array(i, k)) =>
        val (newMarked, newTotal) =
          if marked.contains(i) then (marked, total)
          else (marked + i, total - nums(i))
        Iterator
          .iterate((newMarked, newTotal, k)) { case (marked, total, k) =>
            val (n, i) = pqueue.dequeue()
            if marked.contains(i) then (marked, total, k)
            else (marked + i, total - n, k - 1)
          }
          .dropWhile { case (_, _, k) => k > 0 && pqueue.nonEmpty }.next()
          .pipe { case (marked, total, _) => (ans :+ total, marked, total) }
      }
      .pipe { case (ans, _, _) => ans }
