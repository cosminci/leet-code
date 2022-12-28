package com.leetcode.cosminci._2000

import scala.collection.mutable

object _1962_RemoveStonesToMinimizeTheTotal:

  def minStoneSum(piles: Array[Int], k: Int): Int =
    val largestStones = mutable.PriorityQueue.from(piles)
    piles.sum - (1 to k).foldLeft(0) { case (stonesRemoved, _) =>
      val largestStone = largestStones.dequeue()
      largestStones.enqueue(largestStone - largestStone / 2)
      stonesRemoved + (largestStone / 2)
    }
