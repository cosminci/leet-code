package io.github.cosminci.leetcode._100

import scala.annotation.tailrec

object _6_ZigZagConversion:
  def main(args: Array[String]): Unit =
    println(convert("PAYPALISHIRING", 4))

  def convert(s: String, numRows: Int): String =
    if numRows == 1 then return s
    val indices = s.indices

    def select(startIdx: Int) = indices.filter(i => (i - startIdx) % ((numRows - 1) * 2) == 0)
    def intersperse[A](a: Seq[A], b: Seq[A]): Seq[A] =
      if a.isEmpty then b
      else intersperse(b, a.tail).prepended(a.head)

    val firstRow = select(startIdx = 0)
    val lastRow  = select(startIdx = numRows - 1)
    val midRows = (1 until numRows - 1).map { i =>
      val firstChars  = select(startIdx = i)
      val secondChars = firstChars.map(_ + (numRows - i - 1) * 2).filter(_ < s.length)
      intersperse(firstChars, secondChars)
    }

    midRows.prepended(firstRow).appended(lastRow).flatten.map(s).mkString
