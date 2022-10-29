package com.leetcode.cosminci._600

object _566_ReshapeTheMatrix:
  def main(args: Array[String]): Unit =
    println(matrixReshape(Array(Array(1), Array(2), Array(3), Array(4)), 2, 2).map(_.toList).toList)

  def matrixReshape(mat: Array[Array[Int]], r: Int, c: Int): Array[Array[Int]] =
    if r * c != mat.length * mat(0).length then return mat

    val result = Array.ofDim[Int](r, c)
    mat.indices.foreach { row =>
      mat(0).indices.foreach { col =>
        val idx    = row * mat(0).length + col
        val newRow = idx / c
        val newCol = idx % c
        result(newRow)(newCol) = mat(row)(col)
      }
    }
    result
