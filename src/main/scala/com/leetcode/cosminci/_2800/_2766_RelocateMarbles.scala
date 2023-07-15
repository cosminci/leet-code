package com.leetcode.cosminci._2800

object _2766_RelocateMarbles:

  def relocateMarbles(nums: Array[Int], moveFrom: Array[Int], moveTo: Array[Int]): List[Int] =
    val pos = nums.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)
    moveFrom.zip(moveTo)
      .filterNot { case (from, to) => from == to }
      .foldLeft(pos) { case (pos, (from, to)) => pos.updated(from, 0).updated(to, pos(to) + pos(from)) }
      .collect { case (pos, cnt) if cnt > 0 => pos }
      .toList.sorted
