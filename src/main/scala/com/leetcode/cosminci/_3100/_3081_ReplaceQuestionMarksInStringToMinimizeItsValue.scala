package com.leetcode.cosminci._3100

import scala.collection.mutable

object _3081_ReplaceQuestionMarksInStringToMinimizeItsValue:

  def minimizeStringValue(s: String): String =
    val counter = s.groupMapReduce(identity)(_ => 1)(_ + _)
    val k       = s.count(_ == '?')
    val letters = ('a' to 'z').map(ch => counter.getOrElse(ch, 0) -> ch)
    val pqueue  = mutable.PriorityQueue.from(letters)(Ordering.by { case (cnt, ch) => (-cnt, -ch) })

    @annotation.tailrec
    def dfs(k: Int, replacements: Vector[Char]): Vector[Char] =
      if k == 0 then replacements
      else
        val (cnt, ch) = pqueue.dequeue()
        pqueue.enqueue(cnt + 1 -> ch)
        dfs(k - 1, replacements :+ ch)

    val replacements = dfs(k, Vector.empty).sorted.iterator
    s.map(ch => if ch == '?' then replacements.next() else ch)
