package com.leetcode.cosminci._2700

object _2660_DetermineWinnerOfBowlingGame:

  def isWinner(player1: Array[Int], player2: Array[Int]): Int =
    val (score1, score2) = (score(player1), score(player2))
    if score1 > score2 then 1 else if score1 < score2 then 2 else 0

  private def score(scores: Array[Int]) =
    scores.toSeq match
      case Seq(score)          => score
      case Seq(score1, score2) => if score1 == 10 then score1 + 2 * score2 else score1 + score2
      case score1 +: score2 +: _ =>
        val initScore = if score1 == 10 then score1 + 2 * score2 else score1 + score2
        scores.sliding(3).foldLeft(initScore) { case (score, Array(prev2, prev1, curr)) =>
          if prev2 == 10 || prev1 == 10 then score + curr * 2 else score + curr
        }
