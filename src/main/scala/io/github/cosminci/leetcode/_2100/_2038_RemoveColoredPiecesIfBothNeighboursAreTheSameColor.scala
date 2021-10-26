package io.github.cosminci.leetcode._2100

object _2038_RemoveColoredPiecesIfBothNeighboursAreTheSameColor:
  def main(args: Array[String]): Unit =
    println(winnerOfGame("ABBBBBBBAAA"))

  private def winnerOfGame(colors: String): Boolean =
    val (finalScoreA, finalScoreB, _, _) = colors.foldLeft(0, 0, 0, 0) {
      case ((scoreA, scoreB, colorAStreak, colorBStreak), color) =>
        color match
          case 'A' if colorAStreak >= 2 =>
            (scoreA + 1, scoreB, colorAStreak + 1, 0)
          case 'A' =>
            (scoreA, scoreB, colorAStreak + 1, 0)
          case _ if colorBStreak >= 2 =>
            (scoreA, scoreB + 1, 0, colorBStreak + 1)
          case _ =>
            (scoreA, scoreB, 0, colorBStreak + 1)
    }
    finalScoreA > finalScoreB
