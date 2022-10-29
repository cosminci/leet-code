package com.leetcode.cosminci._1400

import scala.collection.mutable

object _1345_JumpGameIV:
  def minJumps(arr: Array[Int]): Int =
    val positions = arr.indices.groupBy(i => arr(i)).withDefaultValue(Seq.empty)

    val toVisit        = mutable.Queue((0, 0))
    val visitedValues  = mutable.Set.empty[Int]
    val visitedIndices = mutable.Set.empty[Int]

    while toVisit.nonEmpty do
      val (curr, steps) = toVisit.dequeue()
      if curr == arr.length - 1 then return steps

      val nextIndices = Seq(curr - 1, curr + 1) ++
        Option.when(!visitedValues.contains(arr(curr)))(positions(arr(curr))).getOrElse(Seq.empty)

      nextIndices
        .filter(i => i > 0 && i < arr.length && !visitedIndices.contains(i))
        .foreach { next =>
          visitedIndices.add(next)
          toVisit.enqueue((next, steps + 1))
        }

      visitedValues.add(arr(curr))

    Int.MaxValue
