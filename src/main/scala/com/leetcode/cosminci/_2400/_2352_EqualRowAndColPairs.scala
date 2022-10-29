package com.leetcode.cosminci._2400

object _2352_EqualRowAndColPairs:

  def equalPairs(grid: Array[Array[Int]]): Int = {
    val rowHashCounter = grid.map(_.toSeq.hashCode()).groupMapReduce(identity)(_ => 1)(_ + _)
    grid.transpose.map(col => rowHashCounter.getOrElse(col.toSeq.hashCode(), 0)).sum
  }
