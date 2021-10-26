package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _295_FindMedianFromDataStream:

  def main(args: Array[String]): Unit =
    val medianFinder = new MedianFinder
    List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).foreach { n =>
      medianFinder.addNum(n)
      println(medianFinder.findMedian())
    }
    List(6, 10, 2, 6, 5, 0, 6, 3, 1, 0, 0).foreach { n =>
      medianFinder.addNum(n)
      println(medianFinder.findMedian())
    }

  class MedianFinder():

    private val lowerHalf = mutable.PriorityQueue.empty[Int]
    private val upperHalf =
      given Ordering[Int] = (x, y) => y.compare(x)
      mutable.PriorityQueue.empty[Int]

    def addNum(num: Int) =
      if lowerHalf.headOption.exists(_ >= num) then lowerHalf.enqueue(num)
      else if upperHalf.headOption.exists(_ < num) then upperHalf.enqueue(num)
      else lowerHalf.enqueue(num)

      while lowerHalf.size > upperHalf.size + 1 do upperHalf.enqueue(lowerHalf.dequeue())
      while upperHalf.size > lowerHalf.size + 1 do lowerHalf.enqueue(upperHalf.dequeue())

    def findMedian(): Double =
      if lowerHalf.size == upperHalf.size then (lowerHalf.head + upperHalf.head) / 2.0
      else if lowerHalf.size > upperHalf.size then lowerHalf.head
      else upperHalf.head
