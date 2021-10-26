package io.github.cosminci.leetcode._600

import scala.collection.mutable

object _529_Minesweeper:
  private def updateBoard(board: Array[Array[Char]], click: Array[Int]): Array[Array[Char]] =
    val Array(r, c) = click
    if board(r)(c) == 'M' then
      board(r)(c) = 'X'
      return board

    val visited = mutable.Set((r, c))
    if board(r)(c) == 'E' then
      val toVisit = mutable.Queue((r, c))
      while toVisit.nonEmpty do
        val coord @ (x, y) = toVisit.dequeue()
        neighbours(x, y, board) match
          case Left(mineCount) =>
            board(x)(y) = ('0' + mineCount).toChar
          case Right(emptyAdjancent) =>
            board(x)(y) = 'B'
            emptyAdjancent.foreach { adj =>
              if !visited.contains(adj) then
                visited.add(adj)
                toVisit.enqueue(adj)
            }
    board

  def neighbours[T](x: Int, y: Int, grid: Array[Array[T]]): Either[Int, Seq[(Int, Int)]] =
    val above       = Option.when(x > 0)((x - 1, y))
    val left        = Option.when(y > 0)((x, y - 1))
    val right       = Option.when(y < grid(x).length - 1)((x, y + 1))
    val below       = Option.when(x < grid.length - 1)((x + 1, y))
    val topLeft     = Option.when(x > 0 & y > 0)((x - 1, y - 1))
    val topRight    = Option.when(x > 0 & y < grid(x).length - 1)((x - 1, y + 1))
    val bottomLeft  = Option.when(x < grid.length - 1 & y > 0)((x + 1, y - 1))
    val bottomRight = Option.when(x < grid.length - 1 & y < grid(x).length - 1)((x + 1, y + 1))
    val valid       = List(above, left, right, below, topLeft, topRight, bottomLeft, bottomRight).flatten
    val mines       = valid.filter(coord => grid(coord._1)(coord._2) == 'M')
    Either.cond(mines.size == 0, valid, mines.size)
