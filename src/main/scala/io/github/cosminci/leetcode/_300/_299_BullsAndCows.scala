package io.github.cosminci.leetcode._300

object _299_BullsAndCows:
  def main(args: Array[String]): Unit =
    println(getHint("1807", "7810"))
    println(getHint("1123", "0111"))
    println(getHint("1", "0"))
    println(getHint("1", "1"))

  def getHint(secret: String, guess: String): String =
    val (numBulls, secretCounts, guessCounts) =
      secret.zip(guess).zipWithIndex
        .foldLeft(0, Seq.fill(10)(0), Seq.fill(10)(0)) {
          case ((bulls, sCounts, gCounts), ((sChar, gChar), idx)) =>
            val (s, g) = (sChar - '0', gChar - '0')
            if s == g then (bulls + 1, sCounts, gCounts)
            else (bulls, sCounts.updated(s, sCounts(s) + 1), gCounts.updated(g, gCounts(g) + 1))
        }

    val numCows = secretCounts.zip(guessCounts).foldLeft(0) {
      case (cows, (secretCount, guessCount)) =>
        cows + math.min(secretCount, guessCount)
    }

    s"${numBulls}A${numCows}B"
