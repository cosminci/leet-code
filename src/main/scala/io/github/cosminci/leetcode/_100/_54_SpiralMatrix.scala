package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _54_SpiralMatrix:
  def main(args: Array[String]): Unit =
    println(spiralOrder(Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8), Array(9, 10, 11, 12))))

  private def spiralOrder(matrix: Array[Array[Int]]): List[Int] =
    if matrix.length == 1 then return matrix.head.toList
    matrix.head.toList ++ spiralOrder(rotateMinus90(matrix.tail))

  private def transpose(matrix: Array[Array[Int]]): Array[Array[Int]] =
    matrix.head.indices.map(col => matrix.map(_(col))).toArray

  private def rotateMinus90(matrix: Array[Array[Int]]): Array[Array[Int]] =
    val t = transpose(matrix)
    t.head.indices.foreach { col =>
      (0 until t.length / 2).foreach { row =>
        val temp = t(row)(col)
        t(row)(col) = t(t.length - row - 1)(col)
        t(t.length - row - 1)(col) = temp
      }
    }
    t
