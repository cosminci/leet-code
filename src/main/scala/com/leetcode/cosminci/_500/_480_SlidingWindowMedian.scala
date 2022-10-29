package com.leetcode.cosminci._500

import java.util.{Comparator, TreeSet as JTreeSet}
import scala.util.Try

object _480_SlidingWindowMedian:
  def main(args: Array[String]): Unit =
    println(medianSlidingWindow(Array(5, 2, 2, 7, 3, 7, 9, 0, 2, 3), 9).toList)
    println(medianSlidingWindow(Array(2147483647, 2147483647), 2).toList)
    println(medianSlidingWindow(Array(1, 3, -1, -3, 5, 3, 6, 7), 3).toList)
    println(medianSlidingWindow(Array(1, 2, 3, 4, 2, 3, 1, 4, 2), 3).toList)

  def medianSlidingWindow(nums: Array[Int], k: Int): Array[Double] =
    val comparator: Comparator[Int] = (i, j) =>
      if nums(i) != nums(j) then nums(i).compare(nums(j))
      else i.compare(j)
    val lowerHalf = new JTreeSet[Int](comparator.reversed())
    val upperHalf = new JTreeSet[Int](comparator)

    def median(): Double =
      if k % 2 == 0 then (nums(lowerHalf.first()).toDouble + nums(upperHalf.first())) / 2
      else nums(upperHalf.first()).toDouble

    def rebalance(): Unit =
      while lowerHalf.size() > upperHalf.size() do upperHalf.add(lowerHalf.pollFirst())

    nums.indices.take(k).foreach(lowerHalf.add)
    rebalance()

    (k until nums.length)
      .foldLeft(List(median())) { case (acc, i) =>
        if !lowerHalf.remove(i - k) then upperHalf.remove(i - k)

        upperHalf.add(i)
        lowerHalf.add(upperHalf.pollFirst())
        rebalance()

        acc :+ median()
      }
      .toArray
