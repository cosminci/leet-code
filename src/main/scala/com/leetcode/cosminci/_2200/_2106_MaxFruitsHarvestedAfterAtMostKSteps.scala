package com.leetcode.cosminci._2200

object _2106_MaxFruitsHarvestedAfterAtMostKSteps:
  def main(args: Array[String]): Unit =
    println(maxTotalFruits(Array(Array(0, 9), Array(3, 4), Array(4, 2), Array(5, 7), Array(6, 1), Array(10, 9)), 5, 4))

  def maxTotalFruits(fruitsArr: Array[Array[Int]], startPos: Int, k: Int): Int =
    val fruits = fruitsArr.map { case Array(position, amount) => position -> amount }.toMap

    val rightBound = startPos.max(fruits.keys.max)
    val prefixSum = (0 to rightBound).scanLeft(0)((count, position) => count + fruits.getOrElse(position, 0)).tail

    def collectLeft(moves: Int) =
      prefixSum(startPos) - Option.when(startPos - moves > 0)(prefixSum(startPos - moves - 1)).getOrElse(0)

    def collectRight(moves: Int) =
      Option.when(startPos + moves < rightBound)(prefixSum(startPos + moves)).getOrElse(prefixSum.last) -
        Option.when(startPos > 0)(prefixSum(startPos - 1)).getOrElse(0)

    (0 to k / 2).flatMap(moves => Seq(
      collectLeft(moves) + collectRight((k - 2 * moves).max(0)) - fruits.getOrElse(startPos, 0),
      collectRight(moves) + collectLeft((k - 2 * moves).max(0)) - fruits.getOrElse(startPos, 0)
    )).max
