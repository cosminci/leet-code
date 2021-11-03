package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _692_TopKFrequentWords:
  def main(args: Array[String]): Unit =
    println(topKFrequentHeap(Array("i", "love", "leetcode", "i", "love", "coding"), 2))
    println(topKFrequentSort(Array("i", "love", "leetcode", "i", "love", "coding"), 2))
    println(topKFrequentHeap(Array("the", "day", "is", "sunny", "the", "the", "the", "sunny", "is", "is"), 4))
    println(topKFrequentSort(Array("the", "day", "is", "sunny", "the", "the", "the", "sunny", "is", "is"), 4))

  def topKFrequentHeap(words: Array[String], k: Int): List[String] =
    val freqCounts = words.groupBy(identity).view.mapValues(_.length).toMap
    val freqHeap =
      given Ordering[String] = (x, y) =>
        val freqOrdering = freqCounts(x).compare(freqCounts(y))
        if freqOrdering != 0 then freqOrdering else y.compare(x)
      mutable.PriorityQueue.from(freqCounts.keys)

    (1 to k).foldLeft(List.empty[String]) { case (results, _) =>
      results :+ freqHeap.dequeue()
    }

  def topKFrequentSort(words: Array[String], k: Int): List[String] =
    words
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .toSeq
      .sortBy((word, count) => (-count, word))
      .take(k)
      .map(_._1)
      .toList
