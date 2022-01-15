package io.github.cosminci.leetcode._2200

object _2133_CheckIfEveryRowAndColContainsAllNums:

  def checkValid(matrix: Array[Array[Int]]): Boolean =
    (matrix ++ matrix.transpose).forall(_.toSet.size == matrix.length)