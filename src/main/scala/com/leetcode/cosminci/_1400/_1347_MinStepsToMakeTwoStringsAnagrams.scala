package com.leetcode.cosminci._1400

object _1347_MinStepsToMakeTwoStringsAnagrams:

  def minSteps(s: String, t: String): Int =
    s.diff(t).length
