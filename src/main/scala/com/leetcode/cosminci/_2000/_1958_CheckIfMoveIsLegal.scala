package com.leetcode.cosminci._2000

object _1958_CheckIfMoveIsLegal:
  def main(args: Array[String]): Unit =
    println(
      checkMove(
        Array(
          Array('.', '.', '.', 'B', '.', '.', '.', '.'),
          Array('.', '.', '.', 'W', '.', '.', '.', '.'),
          Array('.', '.', '.', 'W', '.', '.', '.', '.'),
          Array('.', '.', '.', 'W', '.', '.', '.', '.'),
          Array('W', 'B', 'B', '.', 'W', 'W', 'W', 'B'),
          Array('.', '.', '.', 'B', '.', '.', '.', '.'),
          Array('.', '.', '.', 'B', '.', '.', '.', '.'),
          Array('.', '.', '.', 'W', '.', '.', '.', '.')
        ),
        4,
        3,
        'B'
      )
    )

  def checkMove(board: Array[Array[Char]], xTarget: Int, yTarget: Int, color: Char): Boolean =
    val directions = Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

    def isLegal(direction: (Int, Int)): Boolean =
      val (xMove, yMove) = direction
      var (x, y)         = (xTarget + xMove, yTarget + yMove)
      var length         = 1

      while x < board.length && x >= 0 && y < board.head.length && y >= 0 do
        length += 1
        if board(x)(y) == '.' then return false
        if board(x)(y) == color then return length >= 3
        x += xMove
        y += yMove
      false

    directions.exists(isLegal)
