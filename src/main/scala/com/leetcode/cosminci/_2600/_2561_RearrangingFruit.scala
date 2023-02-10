package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2561_RearrangingFruit:

  def minCost(basket1: Array[Int], basket2: Array[Int]): Long =
    basket2
      .foldLeft(basket1.groupMapReduce(identity)(_ => 1)(_ + _)) { (counter, n) =>
        counter.updated(n, counter.getOrElse(n, 0) - 1)
      }
      .flatMap { case (n, cnt) =>
        if cnt.abs % 2 == 1 then return -1
        else Seq.fill(cnt.abs / 2)(n)
      }
      .pipe { swaps =>
        val smallest = basket1.min.min(basket2.min)
        swaps.toSeq.sorted.take(swaps.size / 2).map(_.min(smallest * 2).toLong).sum
      }
