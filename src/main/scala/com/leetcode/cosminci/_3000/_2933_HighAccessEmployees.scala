package com.leetcode.cosminci._3000

import scala.util.chaining.*

object _2933_HighAccessEmployees:

  def findHighAccessEmployees(accessTimes: List[List[String]]): List[String] =
    def isHighAccess(accessMinutes: List[Int]) =
      accessMinutes.sorted.pipe(mins => mins.zip(mins.drop(2)).exists { case (x, y) => y - x < 60 })

    accessTimes
      .groupMap(_.head)(l => l.last.take(2).toInt * 60 + l.last.takeRight(2).toInt)
      .collect { case (employee, accessMinutes) if isHighAccess(accessMinutes) => employee }
      .toList
