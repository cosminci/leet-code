package com.leetcode.cosminci._2600

object _2525_CategorizeBox:

  def categorizeBox(length: Int, width: Int, height: Int, mass: Int): String =
    val dimensions = Seq(length, width, height).map(_.toLong)

    val bulky = dimensions.exists(_ >= 10_000) || dimensions.product >= 1_000_000_000
    val heavy = mass >= 100

    if bulky && heavy then "Both"
    else if bulky then "Bulky"
    else if heavy then "Heavy"
    else "Neither"
