package com.leetcode.cosminci._2600

import scala.collection.mutable
import scala.util.chaining.*

object _2532_TimeToCrossBridge:

  def findCrossingTime(n: Int, k: Int, time: Array[Array[Int]]): Int =
    val warehouseLeft  = mutable.TreeSet.empty[(Int, Int)]
    val warehouseRight = mutable.TreeSet.empty[(Int, Int)]

    val initBridgeLeft = time.zipWithIndex.map { case (Array(l2r, _, r2l, _), i) => (l2r + r2l, i) }
    val bridgeLeft     = mutable.PriorityQueue.from(initBridgeLeft)
    val bridgeRight    = mutable.PriorityQueue.empty[(Int, Int)]

    var minute     = 0
    var cratesLeft = n

    while cratesLeft > 0 || bridgeRight.nonEmpty || warehouseRight.nonEmpty do
      while warehouseLeft.headOption.exists { case (t, _) => t <= minute } do
        val (i, Array(l2r, _, r2l, _)) = warehouseLeft.head.pipe { case (_, i) => (i, time(i)) }
        warehouseLeft.remove(warehouseLeft.head)
        bridgeLeft.enqueue((l2r + r2l, i))
      while warehouseRight.headOption.exists { case (t, _) => t <= minute } do
        val (i, Array(l2r, _, r2l, _)) = warehouseRight.head.pipe { case (_, i) => (i, time(i)) }
        warehouseRight.remove(warehouseRight.head)
        bridgeRight.enqueue((l2r + r2l, i))
      if bridgeRight.nonEmpty then
        val (i, Array(_, _, r2l, rwh)) = bridgeRight.dequeue().pipe { case (_, i) => (i, time(i)) }
        minute += r2l
        warehouseLeft.addOne((minute + rwh, i))
      else if bridgeLeft.nonEmpty && cratesLeft > 0 then
        val (i, Array(l2r, lwh, _, _)) = bridgeLeft.dequeue().pipe { case (_, i) => (i, time(i)) }
        minute += l2r
        cratesLeft -= 1
        warehouseRight.addOne((minute + lwh, i))
      else
        minute = warehouseRight.headOption
          .map { case (t, _) => t }
          .getOrElse(Int.MaxValue)
          .min(warehouseLeft.headOption.map { case (t, _) => t }.filter(_ => cratesLeft > 0).getOrElse(Int.MaxValue))

    minute
