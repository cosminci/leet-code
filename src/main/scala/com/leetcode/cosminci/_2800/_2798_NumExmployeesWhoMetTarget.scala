package com.leetcode.cosminci._2800

object _2798_NumExmployeesWhoMetTarget:

  def numberOfEmployeesWhoMetTarget(hours: Array[Int], target: Int): Int =
    hours.count(_ >= target)
