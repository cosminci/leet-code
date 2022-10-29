package com.leetcode.cosminci._700

import scala.collection.mutable
import scala.util.chaining.*

object _692_TopKFrequentWords:

  def topKFrequentSort(words: Array[String], k: Int): List[String] =
    words
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toList
      .sortBy { case (word, count) => (-count, word) }
      .take(k)
      .map { case (word, _) => word }

  def topKFrequentHeap(words: Array[String], k: Int): List[String] =
    val counts = words.groupMapReduce(identity)(_ => 1)(_ + _)
    val pqueue = mutable.PriorityQueue.from(counts.keys) { (w1: String, w2: String) =>
      counts(w1).compare(counts(w2)).pipe(res => if res != 0 then res else w2.compare(w1))
    }
    (1 to k).foldLeft(Seq.empty[String])((results, _) => results :+ pqueue.dequeue()).toList
