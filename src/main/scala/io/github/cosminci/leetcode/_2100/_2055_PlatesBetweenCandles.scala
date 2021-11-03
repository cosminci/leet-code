package io.github.cosminci.leetcode._2100

object _2055_PlatesBetweenCandles:
  def main(args: Array[String]): Unit =
    println(platesBetweenCandles("*|*", Array(Array(2, 2))).toSeq)
    println(platesBetweenCandles("**|**|***|", Array(Array(2, 5), Array(5, 9))).toSeq)
    println(
      platesBetweenCandles(
        "***|**|*****|**||**|*",
        Array(Array(1, 17), Array(4, 5), Array(14, 17), Array(5, 11), Array(15, 16))
      ).toSeq
    )

  def platesBetweenCandles(s: String, queries: Array[Array[Int]]): Array[Int] = {
    val prefixCount = s.scanLeft(0) { case (count, pos) => if (pos == '|') count + 1 else count }
    val prevCandle  = s.indices.scanLeft(0) { case (prev, i) => if (s(i) == '|') i else prev }
    val nextCandle  = s.indices.scanRight(s.length) { case (i, next) => if (s(i) == '|') i else next }

    queries.map { case Array(left, right) =>
      val leftCandle  = nextCandle(left)
      val rightCandle = prevCandle(right + 1)
      val candles     = prefixCount(rightCandle) - prefixCount(leftCandle)
      Option.when(leftCandle < rightCandle)(rightCandle - leftCandle - candles).getOrElse(0)
    }
  }
