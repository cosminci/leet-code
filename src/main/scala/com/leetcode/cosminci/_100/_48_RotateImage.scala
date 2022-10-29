package com.leetcode.cosminci._100

object _48_RotateImage:
  def main(args: Array[String]): Unit =
    val input = Array(Array(5, 1, 9, 11), Array(2, 4, 8, 10), Array(13, 3, 6, 7), Array(15, 14, 12, 16))
    rotateFourGroups(input)
    println(input.map(_.toList).toList)

  def rotateFourGroups(matrix: Array[Array[Int]]): Unit =
    def rotateLayer(layer: Int) =
      (layer until matrix.length - 1 - layer).foreach { offset =>
        val tmp = matrix(layer)(offset)
        matrix(layer)(offset) = matrix(matrix.length - 1 - offset)(layer)
        matrix(matrix.length - 1 - offset)(layer) = matrix(matrix.length - 1 - layer)(matrix.length - 1 - offset)
        matrix(matrix.length - 1 - layer)(matrix.length - 1 - offset) = matrix(offset)(matrix.length - 1 - layer)
        matrix(offset)(matrix.length - 1 - layer) = tmp
      }

    (0 until matrix.length / 2).foreach(rotateLayer)

  def rotateTransposeReflect(matrix: Array[Array[Int]]): Unit =
    (0 until matrix.length - 1).foreach { row =>
      (row + 1 until matrix.length).foreach { col =>
        val tmp = matrix(row)(col)
        matrix(row)(col) = matrix(col)(row)
        matrix(col)(row) = tmp
      }
    }
    matrix.indices.foreach { row =>
      (0 until matrix.length / 2).foreach { col =>
        val tmp = matrix(row)(col)
        matrix(row)(col) = matrix(row)(matrix.length - 1 - col)
        matrix(row)(matrix.length - 1 - col) = tmp
      }
    }
