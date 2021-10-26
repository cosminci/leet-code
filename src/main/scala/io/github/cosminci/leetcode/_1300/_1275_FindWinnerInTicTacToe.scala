package io.github.cosminci.leetcode._1300

object _1275_FindWinnerInTicTacToe:
  private def tictactoeBruteForce(moves: Array[Array[Int]]): String =
    val grid = Array.fill(3, 3)(' ')

    def winConditionAchieved(row: Int, col: Int, sign: Char): Boolean =
      (0 to 2).forall(c => grid(row)(c) == sign) ||
        (0 to 2).forall(r => grid(r)(col) == sign) ||
        (row == col && (0 to 2).forall(i => grid(i)(i) == sign)) ||
        (row == 2 - col && (0 to 2).forall(i => grid(i)(2 - i) == sign))

    moves.zipWithIndex.foreach { case (Array(r, c), i) =>
      val (player, sign) = if i % 2 == 0 then ("A", 'X') else ("B", 'O')
      grid(r)(c) = sign
      if winConditionAchieved(r, c, sign) then return player
    }

    if moves.length == 9 then "Draw" else "Pending"

  private def tictactoeRecordMoves(moves: Array[Array[Int]]): String =
    val (rowBalance, colBalance)     = (Array.ofDim[Int](3), Array.ofDim[Int](3))
    var (diag1Balance, diag2Balance) = (0, 0)

    moves.zipWithIndex.foreach { case (Array(r, c), i) =>
      val (player, balance) = if i % 2 == 0 then ("A", 1) else ("B", -1)
      rowBalance(r) += balance
      colBalance(c) += balance

      if r == c then diag1Balance += balance
      if r + c == 2 then diag2Balance += balance

      if rowBalance.exists(rb => math.abs(rb) == 3) ||
        colBalance.exists(cb => math.abs(cb) == 3) ||
        math.abs(diag1Balance) == 3 ||
        math.abs(diag2Balance) == 3
      then return player
    }

    if moves.length == 9 then "Draw" else "Pending"
