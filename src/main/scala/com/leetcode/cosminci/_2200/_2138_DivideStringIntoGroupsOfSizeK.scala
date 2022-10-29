package com.leetcode.cosminci._2200

object _2138_DivideStringIntoGroupsOfSizeK:

  def divideString(s: String, k: Int, fill: Char): Array[String] =
    s.padTo((s.length + k - 1) / k * k, fill).grouped(k).toArray
