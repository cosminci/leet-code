package com.leetcode.cosminci._2500

object _2432_EmployeeThatWorkedOnLongestTask:

  def hardestWorker(n: Int, logs: Array[Array[Int]]): Int =
    logs.tail
      .foldLeft(logs.head.head, logs.head.last, logs.head.last) {
        case ((result, maxDuration, prevEnd), Array(employee, newEnd)) =>
          if newEnd - prevEnd > maxDuration then (employee, newEnd - prevEnd, newEnd)
          else if newEnd - prevEnd < maxDuration then (result, maxDuration, newEnd)
          else (result.min(employee), maxDuration, newEnd)
      }
      ._1
