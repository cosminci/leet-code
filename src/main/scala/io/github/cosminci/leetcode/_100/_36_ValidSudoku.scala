package io.github.cosminci.leetcode._100

object _36_ValidSudoku:

  def main(args: Array[String]): Unit =
    println(
      isValidSudoku(
        Array(
          Array('5', '3', '.', '.', '7', '.', '.', '.', '.'),
          Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
          Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
          Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
          Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
          Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
          Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
          Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
          Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
        )
      )
    )

  def isValidSudoku(board: Array[Array[Char]]): Boolean =
    val rowBitArray    = Array.ofDim[Boolean](9, 9)
    val colBitArray    = Array.ofDim[Boolean](9, 9)
    val squareBitArray = Array.ofDim[Boolean](9, 9)

    board.indices.foreach { row =>
      board(row).indices.foreach { col =>
        val v = board(row)(col) - '0'
        if v >= 0 && v <= 9 then
          if rowBitArray(row)(v - 1) then return false
          else rowBitArray(row)(v - 1) = true

          if colBitArray(col)(v - 1) then return false
          else colBitArray(col)(v - 1) = true

          val squareIdx = (row / 3) * 3 + (col / 3)
          if squareBitArray(squareIdx)(v - 1) then return false
          else squareBitArray(squareIdx)(v - 1) = true
      }
    }
    true
