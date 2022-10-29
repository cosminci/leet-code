package com.leetcode.cosminci._200

object _118_PascalsTriangle:

  def generate(numRows: Int): List[List[Int]] =
    Iterator
      .iterate(Seq(1))(prev => 1 +: (1 until prev.length).map(i => prev(i - 1) + prev(i)) :+ 1)
      .take(numRows)
      .map(_.toList)
      .toList
