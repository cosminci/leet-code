package com.leetcode.cosminci._1500

object _1491_AvgASalaryExcludingMinAndMax:

  def average(salary: Array[Int]): Double =
    salary.diff(Seq(salary.min, salary.max)).sum.toDouble / (salary.length - 2)
