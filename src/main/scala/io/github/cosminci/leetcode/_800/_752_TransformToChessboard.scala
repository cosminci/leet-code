package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _752_TransformToChessboard:
  def main(args: Array[String]): Unit =
    println(
      movesToChessboard(
        Array(
          Array(0, 1, 1, 0),
          Array(0, 1, 1, 0),
          Array(1, 0, 0, 1),
          Array(1, 0, 0, 1)
        )
      )
    )

  private def movesToChessboard(board: Array[Array[Int]]): Int =
    val n = board.length
    for
      i <- 0 until n
      j <- 0 until n
      if ((board(0)(0) ^ board(i)(0)) ^ (board(0)(j) ^ board(i)(j))) != 0
    do return -1

    val (rowSum, colSum, rowMisplaced, colMisplaced) = (0 until n).foldLeft(0, 0, 0, 0) {
      case ((prevRowSum, prevColSum, prevRowMisplaced, prevColMisplaced), i) =>
        (
          prevRowSum + board(0)(i),
          prevColSum + board(i)(0),
          if board(i)(0) == i % 2 then prevRowMisplaced + 1 else prevRowMisplaced,
          if board(0)(i) == i % 2 then prevColMisplaced + 1 else prevColMisplaced
        )
    }

    if rowSum != n / 2 && rowSum != (n + 1) / 2 then return -1
    if colSum != n / 2 && colSum != (n + 1) / 2 then return -1

    if n % 2 == 0 then
      (math.min(n - colMisplaced, colMisplaced) + math.min(n - rowMisplaced, rowMisplaced)) / 2
    else {
      (if colMisplaced % 2 == 1 then n - colMisplaced else colMisplaced) +
      (if rowMisplaced % 2 == 1 then n - rowMisplaced else rowMisplaced)
    } / 2
