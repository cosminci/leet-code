package com.leetcode.cosminci._2800

import scala.collection.mutable

object _2713_MaxStrictlyIncreasingCellsInMatrix:

  def maxIncreasingCells(mat: Array[Array[Int]]): Int =
    val (m, n) = (mat.length, mat.head.length)

    val valueToPos = mutable.TreeMap.empty[Int, Seq[(Int, Int)]].withDefaultValue(Seq.empty)
    for
      i <- 0 until m
      j <- 0 until n
    yield valueToPos.update(mat(i)(j), valueToPos(mat(i)(j)) :+ (i, j))

    val dp  = Array.fill(m, n)(0)
    val res = Array.fill(m + n)(0)
    valueToPos.foreach { case (v, pos) =>
      pos.foreach { case (i, j) =>
        dp(i)(j) = res(i).max(res(m + j)) + 1
      }
      pos.foreach { case (i, j) =>
        res(m + j) = res(m + j).max(dp(i)(j))
        res(i) = res(i).max(dp(i)(j))
      }
    }

    res.max
