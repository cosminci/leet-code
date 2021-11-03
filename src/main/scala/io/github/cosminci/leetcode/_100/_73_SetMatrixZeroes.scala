package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _73_SetMatrixZeroes:
  def main(args: Array[String]): Unit =
    val input = Array(Array(1, 0, 3))
    setZeroes(input)
    println()

  def setZeroes(matrix: Array[Array[Int]]): Unit =
    val nullifyFirstRow = matrix.head.contains(0)
    val nullifyFirstCol = matrix.map(_.head).contains(0)

    matrix.indices.tail.foreach { row =>
      matrix(row).indices.tail.foreach { col =>
        if matrix(row)(col) == 0 then
          matrix(0)(col) = 0
          matrix(row)(0) = 0
      }
    }

    matrix.indices.tail.foreach { row =>
      matrix(row).indices.tail.foreach { col =>
        if matrix(0)(col) == 0 || matrix(row)(0) == 0 then matrix(row)(col) = 0
      }
    }

    if nullifyFirstCol then matrix.indices.foreach(row => matrix(row)(0) = 0)
    if nullifyFirstRow then matrix.head.indices.foreach(col => matrix(0)(col) = 0)
