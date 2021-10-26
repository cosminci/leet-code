package io.github.cosminci.leetcode._2100

object _2001_NumberOfPairsOfInterchangeableRectangles:
  def main(args: Array[String]): Unit =
    println(interchangeableRectangles(Array(Array(4, 8), Array(3, 6), Array(10, 20), Array(15, 30))))
    println(interchangeableRectangles(Array(Array(4, 5), Array(7, 8))))

  private def interchangeableRectangles(rectangles: Array[Array[Int]]): Long =
    rectangles
      .groupBy { case Array(width, height) =>
        width.toDouble / height
      }
      .foldLeft(0L) { case (count: Long, (_, sameRatioRectangles)) =>
        val n = sameRatioRectangles.length
        count + n.toLong * (n - 1) / 2
      }
