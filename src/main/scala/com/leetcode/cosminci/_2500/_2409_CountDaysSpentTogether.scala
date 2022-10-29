package com.leetcode.cosminci._2500

object _2409_CountDaysSpentTogether:

  def countDaysTogether(arriveAlice: String, leaveAlice: String, arriveBob: String, leaveBob: String): Int =
    val days = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    def day(s: String) =
      val Seq(month, day) = s.split('-').toSeq
      (0 until month.toInt - 1).map(days).sum + day.toInt

    val (aliceA, aliceL) = (day(arriveAlice), day(leaveAlice))
    val (bobA, bobL)     = (day(arriveBob), day(leaveBob))
    val (maxA, minL)     = (aliceA.max(bobA), aliceL.min(bobL))

    if maxA > minL then 0 else minL - maxA + 1
