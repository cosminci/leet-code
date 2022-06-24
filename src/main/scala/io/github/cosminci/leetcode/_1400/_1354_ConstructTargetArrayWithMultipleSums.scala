package io.github.cosminci.leetcode._1400

import scala.collection.mutable

object _1354_ConstructTargetArrayWithMultipleSums:

  def isPossible(target: Array[Int]): Boolean =
    val pqueue = mutable.PriorityQueue.from(target)

    @annotation.tailrec
    def dfs(sum: Int): Boolean =
      val elem = pqueue.dequeue()
      if elem == 1 then true
      else if elem == sum then false
      else
        val cand = (elem - 1) % (sum - elem) + 1
        if elem == cand then false
        else
          pqueue.enqueue(cand)
          dfs(sum - elem + cand)

    dfs(target.sum)
