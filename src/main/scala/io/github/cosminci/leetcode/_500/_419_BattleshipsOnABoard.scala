package io.github.cosminci.leetcode._500

object _419_BattleshipsOnABoard:
  private def countBattleships(board: Array[Array[Char]]): Int =
    var count = 0
    for
      x <- board.indices
      y <- board(x).indices
      if board(x)(y) == 'X'
      if x == 0 || board(x - 1)(y) == '.'
      if y == 0 || board(x)(y - 1) == '.'
    do count += 1
    count
