package io.github.cosminci.leetcode._700

object _670_MaxSwap:
  def main(args: Array[String]): Unit =
    println(maximumSwap(98368))
    println(maximumSwap(1993))

  def maximumSwap(num: Int): Int =
    val numDigits = num.toString.map(_ - '0')
    val maxSuffix = numDigits.scanRight(0)((n, max) => math.max(n, max)).tail

    numDigits.indices.collectFirst {
      case idx1 if maxSuffix(idx1) > numDigits(idx1) =>
        (idx1, numDigits.lastIndexOf(maxSuffix(idx1)))
    } match
      case None               => num
      case Some((idx1, idx2)) => numDigits.updated(idx1, numDigits(idx2)).updated(idx2, numDigits(idx1)).mkString.toInt
