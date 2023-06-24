package com.leetcode.cosminci._2800

import scala.collection.mutable

object _2746_DecrementalStringConcat:

  def minimizeConcatenatedLength(words: Array[String]): Int =
    val mem = mutable.Map.empty[(Int, Char, Char), Int]
    def dfs(i: Int, head: Char, last: Char): Int = mem.getOrElseUpdate((i, head, last),
      if i == words.length then 0
      else
        val wordLen     = words(i).length
        val appendCost  = if words(i).head == last then wordLen - 1 else wordLen
        val prependCost = if words(i).last == head then wordLen - 1 else wordLen
        (appendCost + dfs(i + 1, head, words(i).last)).min(prependCost + dfs(i + 1, words(i).head, last))
    )
    val seed = words.head
    seed.length + dfs(i = 1, head = seed.head, last = seed.last)
