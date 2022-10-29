package com.leetcode.cosminci._2500

object _2437_NumValidClockTimes:

  def countTime(time: String): Int =
    val a = if time(0) != '?' then 1 else if Array('0', '1', '2', '3', '?').contains(time.charAt(1)) then 3 else 2
    val b =
      if time(1) != '?' then 1
      else if Array('0', '1').contains(time.head) then 10
      else if time.head == '2' then 4
      else if time.head == '?' then 8
      else 1
    val c = if time(3) != '?' then 1 else 6
    val d = if time(4) != '?' then 1 else 10

    a * b * c * d
