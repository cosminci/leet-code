package com.leetcode.cosminci._2600

import java.util
import scala.util.chaining.*

object _2569_HandlingSumQueriesAfterUpdate:

  def handleQuery(nums1: Array[Int], nums2: Array[Int], queries: Array[Array[Int]]): Array[Long] =
    val bs = new util.BitSet().tap { bs =>
      nums1.indices.foreach(i => if nums1(i) == 1 then bs.set(i))
    }

    queries
      .foldLeft(Array.empty[Long], nums2.map(_.toLong).sum) { case ((results, sum), Array(queryType, a, b)) =>
        if queryType == 1 then bs.flip(a, b + 1).pipe(_ => (results, sum))
        else if queryType == 2 then (results, sum + a.toLong * bs.cardinality())
        else (results :+ sum, sum)
      }
      .pipe { case (results, _) => results }
