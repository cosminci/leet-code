package com.leetcode.cosminci._2700

object _2678_NumSeniorCitizens:

  def countSeniors(details: Array[String]): Int =
    details.count(_.slice(11, 13) > "60")
