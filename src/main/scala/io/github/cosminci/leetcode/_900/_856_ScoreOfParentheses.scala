package io.github.cosminci.leetcode._900

object _856_ScoreOfParentheses:
  def main(args: Array[String]): Unit =
    println(scoreOfParentheses("(()(()))"))

  def scoreOfParentheses(s: String): Int =
    s.foldLeft(Seq.empty[Int], 0) {
      case ((prevScores, currScore), char) =>
        char match {
          case '(' => (prevScores :+ currScore, 0)
          case _   => (prevScores.dropRight(1), prevScores.last + (currScore * 2).max(1))
        }
    }._2
