package com.leetcode.cosminci._2600

import scala.collection.mutable

object _2558_TakeGiftsFromRichestPile:

  def pickGifts(gifts: Array[Int], k: Int): Long =
    val pqueue = mutable.PriorityQueue.from(gifts.map(_.toLong))
    (1 to k).foreach { _ =>
      val gifts = pqueue.dequeue()
      pqueue.enqueue(math.sqrt(gifts).toLong)
    }
    pqueue.sum
