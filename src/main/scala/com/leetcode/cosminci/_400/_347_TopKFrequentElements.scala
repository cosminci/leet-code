package com.leetcode.cosminci._400

import scala.collection.mutable

object _347_TopKFrequentElements:
  def main(args: Array[String]): Unit =
    println(topKFrequentTreeMap(Array(1, 1, 4, 4, 4, 2, 2, 3, 3, 3), 2).toList)
    println(topKFrequentPQueue(Array(1, 1, 4, 4, 4, 2, 2, 3, 3, 3), 2).toList)
    println(topKFrequentPredefBuckets(Array(1, 1, 4, 4, 4, 2, 2, 3, 3, 3), 2).toList)
    println(topKFrequentQuickSelect(Array(1, 1, 4, 4, 4, 2, 2, 3, 3, 3), 2).toList)

  def topKFrequentTreeMap(nums: Array[Int], k: Int): Array[Int] =
    given Ordering[Int] = (x, y) => y.compareTo(x)

    val numToFreq = mutable.Map.empty[Int, Int]
    val freqToNum = mutable.TreeMap(0 -> mutable.Set.empty[Int])
    nums.foreach { n =>
      val prevFreq = numToFreq.getOrElseUpdate(n, 0)
      numToFreq.update(n, prevFreq + 1)
      freqToNum(prevFreq).remove(n)

      val newFreq = prevFreq + 1
      freqToNum.getOrElseUpdate(newFreq, mutable.Set.empty).add(n)
    }
    freqToNum.values.flatten.take(k).toArray

  def topKFrequentPQueue(nums: Array[Int], k: Int): Array[Int] =
    given Ordering[(Int, Int)] = (x, y) => x._2.compareTo(y._2)

    val pqueue  = mutable.PriorityQueue.from(frequencies(nums))
    val results = mutable.ListBuffer.empty[Int]
    while results.size != k do results.addOne(pqueue.dequeue()._1)

    results.toArray

  def topKFrequentPredefBuckets(nums: Array[Int], k: Int): Array[Int] =
    val freqs = Array.fill(nums.length + 1)(mutable.ListBuffer.empty[Int])
    frequencies(nums).foreach { case (n, freq) => freqs(freq).addOne(n) }
    freqs.reverse.flatten.take(k)

  def topKFrequentQuickSelect(nums: Array[Int], k: Int): Array[Int] =
    val freqs  = frequencies(nums).toArray
    val target = freqs.length - k

    def swap(i: Int, j: Int): Unit =
      val tmp = freqs(i)
      freqs(i) = freqs(j)
      freqs(j) = tmp

    @annotation.tailrec
    def fixPivot(start: Int, end: Int): Array[Int] =
      val pivot = freqs(end)._2
      var i     = start
      (i to end).foreach { j =>
        if freqs(j)._2 <= pivot then
          swap(i, j)
          i += 1
      }
      if i - 1 == target then freqs.slice(target, freqs.length).map(_._1)
      else if i - 1 < target then fixPivot(i, end)
      else fixPivot(start, i - 2)

    fixPivot(start = 0, end = freqs.length - 1)

  private def frequencies(nums: Array[Int]): Map[Int, Int] = nums.groupMapReduce(identity)(_ => 1)(_ + _)

