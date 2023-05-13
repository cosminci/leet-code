package com.leetcode.cosminci._2700

object _2672_NumAdjElementsWithSameColor:

  def colorTheArray(n: Int, queries: Array[Array[Int]]): Array[Int] =
    val colors = Array.fill(n)(0)
    queries.scanLeft(0) { case (res, Array(i, color)) =>
      val prevColor = if i > 0 then colors(i - 1) else 0
      val nextColor = if i < n - 1 then colors(i + 1) else 0
      val prevMinus = if colors(i) > 0 && colors(i) == prevColor then 1 else 0
      val nextMinus = if colors(i) > 0 && colors(i) == nextColor then 1 else 0
      val prevPlus  = if color == prevColor then 1 else 0
      val nextPlus  = if color == nextColor then 1 else 0
      colors(i) = color
      res - prevMinus - nextMinus + prevPlus + nextPlus
    }.tail
