package com.leetcode.cosminci._2300

object _2244_MinRoundsToCompleteAllTasks:

  def minimumRounds(tasks: Array[Int]): Int =
    tasks
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .values
      .foldLeft(0) { (rounds, count) =>
        if count == 1 then return -1
        rounds + (count + 2) / 3
      }
