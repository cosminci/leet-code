package com.leetcode.cosminci._800

import scala.collection.mutable

object _767_ReorganizeString:

  def reorganizeString(s: String): String =
    val charCounts = s.groupMapReduce(identity)(_ => 1)(_ + _)
    given Ordering[(Char, Int)] = (x, y) => x._2.compare(y._2)

    val pqueue = mutable.PriorityQueue.from(charCounts)
    val result = new StringBuilder

    var prev: Option[(Char, Int)] = None
    while pqueue.nonEmpty do
      val (mostFrequentChar, count) = pqueue.dequeue()
      result.append(mostFrequentChar)
      prev.foreach(p => pqueue.enqueue(p))
      prev = Option.when(count > 1)((mostFrequentChar, count - 1))

    if result.length() != s.length then ""
    else result.toString
