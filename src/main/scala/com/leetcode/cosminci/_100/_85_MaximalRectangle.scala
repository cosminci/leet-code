package com.leetcode.cosminci._100

object _85_MaximalRectangle:
  def main(args: Array[String]): Unit =
    println(
      maximalRectangle(
        Array(
          Array('1', '1', '1', '1', '1', '1', '1', '1'),
          Array('1', '1', '1', '1', '1', '1', '1', '0'),
          Array('1', '1', '1', '1', '1', '1', '1', '0'),
          Array('1', '1', '1', '1', '1', '0', '0', '0'),
          Array('0', '1', '1', '1', '1', '0', '0', '0')
        )
      )
    )

  def maximalRectangle(matrix: Array[Array[Char]]): Int =
    if matrix.isEmpty then return 0
    val (m, n) = (matrix.length, matrix.head.length)

    (0 until m)
      .foldLeft(Seq.fill(n)(0), Seq.fill(n)(n), Seq.fill(n)(0), 0) {
        case ((prevLeft, prevRight, prevHeight, prevMaxArea), r) =>
          val height = (0 until n).map(c => Option.when(matrix(r)(c) == '1')(prevHeight(c) + 1).getOrElse(0))

          val left = (0 until n).foldLeft(Seq.empty[Int], 0) {
            case ((left, currLeft), c) =>
              if matrix(r)(c) == '1' then (left :+ prevLeft(c).max(currLeft), currLeft)
              else (left :+ 0, c + 1)
          }._1

          val right = (0 until n).foldRight(Seq.empty[Int], n) {
            case (c, (right, currRight)) =>
              if matrix(r)(c) == '1' then (prevRight(c).min(currRight) +: right, currRight)
              else (n +: right, c)
          }._1

          val maxArea = prevMaxArea.max((0 until n).map(c => (right(c) - left(c)) * height(c)).max)

          (left, right, height, maxArea)
      }._4
