package com.leetcode.cosminci._2700

import scala.collection.mutable

object _2678_SumInMatrix:

  def matrixSum(nums: Array[Array[Int]]): Int =
    val pqueues = nums.map(row => mutable.PriorityQueue.from(row))
    Iterator
      .iterate(0)(_ + pqueues.map(_.dequeue()).max)
      .dropWhile(_ => pqueues.head.nonEmpty)
      .next()
