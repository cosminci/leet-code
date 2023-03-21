package com.leetcode.cosminci._2600

import scala.collection.immutable.TreeSet
import scala.util.chaining._

object _2593_FindScoreOfArrayAfterMarkingAllElements:

  def findScore(nums: Array[Int]): Long =
    Iterator
      .iterate((TreeSet.from(nums.zipWithIndex), Set.empty[Int], 0L)) { case (toVisit, marked, res) =>
        val (n, i) = toVisit.head
        if marked.contains(i) then (toVisit.tail, marked, res)
        else (toVisit.tail, marked ++ Seq((i - 1).max(0), i, (i + 1).min(nums.length - 1)), res + n)
      }
      .dropWhile { case (nums, _, _) => nums.nonEmpty }.next()
      .pipe { case (_, _, res) => res }
