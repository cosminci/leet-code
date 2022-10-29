package io.github.cosminci.leetcode._200

object _119_PascalsTriangleII:

  def getRow(rowIndex: Int): List[Int] =
    Iterator
      .iterate(Seq(1))(prev => 1 +: (1 until prev.length).map(i => prev(i - 1) + prev(i)) :+ 1)
      .drop(rowIndex)
      .next()
      .toList
