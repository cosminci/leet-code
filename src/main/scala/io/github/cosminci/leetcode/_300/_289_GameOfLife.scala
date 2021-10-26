package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _289_GameOfLife:

  def main(args: Array[String]): Unit =
    println(
      gameOfLifeTruthTable(Array(Array(0, 1, 0), Array(0, 0, 1), Array(1, 1, 1), Array(0, 0, 0))) ==
        gameOfLifeFlipSet(Array(Array(0, 1, 0), Array(0, 0, 1), Array(1, 1, 1), Array(0, 0, 0)))
    )

  private def gameOfLifeFlipSet(board: Array[Array[Int]]): Unit =
    val toFlip = mutable.Set.empty[(Int, Int)]
    board.indices.foreach { x =>
      board(x).indices.foreach { y =>
        val liveNeighbours = countLiveNeighbours(board, x, y)
        if board(x)(y) == 1 && (liveNeighbours < 2 || liveNeighbours > 3) then toFlip.add((x, y))
        else if board(x)(y) == 0 && liveNeighbours == 3 then toFlip.add((x, y))
      }
    }
    toFlip.foreach { case (x, y) =>
      board(x)(y) = 1 - board(x)(y)
    }

  private def gameOfLifeTruthTable(board: Array[Array[Int]]): Unit =
    board.indices.foreach { x =>
      board(x).indices.foreach { y =>
        val liveNeighbours = countLiveNeighbours(board, x, y)
        if board(x)(y) == 1 then board(x)(y) = if liveNeighbours < 2 || liveNeighbours > 3 then 1 else 3
        else board(x)(y) = if liveNeighbours == 3 then 2 else 0
      }
    }
    board.indices.foreach { x =>
      board(x).indices.foreach { y =>
        if board(x)(y) == 1 then board(x)(y) = 0
        else if board(x)(y) == 2 || board(x)(y) == 3 then board(x)(y) = 1
      }
    }

  private def countLiveNeighbours(board: Array[Array[Int]], x: Int, y: Int): Int =
    Option
      .when(board.isDefinedAt(x - 1)) {
        board(x - 1)(y) % 2 +
          Option.when(board(x - 1).isDefinedAt(y - 1))(board(x - 1)(y - 1) % 2).getOrElse(0) +
          Option.when(board(x - 1).isDefinedAt(y + 1))(board(x - 1)(y + 1) % 2).getOrElse(0)
      }
      .getOrElse(0) +
      Option.when(board(x).isDefinedAt(y - 1))(board(x)(y - 1) % 2).getOrElse(0) +
      Option.when(board(x).isDefinedAt(y + 1))(board(x)(y + 1) % 2).getOrElse(0) +
      Option
        .when(board.isDefinedAt(x + 1)) {
          board(x + 1)(y) % 2 +
            Option.when(board(x + 1).isDefinedAt(y - 1))(board(x + 1)(y - 1) % 2).getOrElse(0) +
            Option.when(board(x + 1).isDefinedAt(y + 1))(board(x + 1)(y + 1) % 2).getOrElse(0)
        }
        .getOrElse(0)
