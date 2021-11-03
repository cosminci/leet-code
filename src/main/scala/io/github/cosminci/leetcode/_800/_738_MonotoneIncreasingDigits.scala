package io.github.cosminci.leetcode._800

object _738_MonotoneIncreasingDigits:
  def main(args: Array[String]): Unit =
    Seq(10, 1234, 332, 1032).foreach { n =>
      println(monotoneIncreasingDigits(n))
    }

  def monotoneIncreasingDigits(n: Int): Int =
    val nDigits = n.toString.map(_ - '0')

    val (result, monotoneEnd) = (nDigits.length - 1 to 1 by -1).foldLeft(nDigits, nDigits.length - 1) {
      case ((digits, end), i) =>
        if digits(i - 1) <= digits(i) then (digits, end)
        else (digits.updated(i - 1, digits(i - 1) - 1), i - 1)
    }

    result.indices.map(i => if i <= monotoneEnd then result(i) else 9).mkString.toInt
