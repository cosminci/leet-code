package com.leetcode.cosminci._900

object _826_MostProfitAssigningWork:

  def main(args: Array[String]): Unit =
    println(maxProfitAssignment(Array(68, 35, 52, 47, 86), Array(67, 17, 1, 81, 3), Array(92, 10, 85, 84, 82)))
    println(maxProfitAssignment(Array(2, 4, 6, 8, 10), Array(10, 20, 30, 40, 50), Array(4, 5, 6, 7)))
    println(maxProfitAssignment(Array(85, 47, 57), Array(24, 66, 99), Array(40, 25, 25)))

  def maxProfitAssignment(difficulties: Array[Int], profits: Array[Int], workers: Array[Int]): Int =
    val jobs = difficulties.zip(profits).sorted

    val bestJobs = jobs.tail.foldLeft(Array(jobs.head)) { 
      case (bestJobsAcc, (difficulty, profit)) =>
        bestJobsAcc :+ (difficulty, bestJobsAcc.last._2.max(profit))
    }

    workers.sorted.map { ability =>
      val bestIdx = bestJobs.search((ability, Int.MaxValue)).insertionPoint
      if bestIdx == 0 then 0
      else if bestIdx == bestJobs.length then bestJobs.last._2
      else bestJobs(bestIdx - 1)._2
    }.sum
