package io.github.cosminci.leetcode._2100

import scala.math.Integral.Implicits.*

object _2028_FindMissingObservations:
  def main(args: Array[String]): Unit =
    println(missingRolls(Array(6, 3, 4, 3, 5, 3), 1, 6).toSeq)
    println(missingRolls(Array(1, 2, 3, 4), 6, 4).toSeq)

  def missingRolls(rolls: Array[Int], mean: Int, n: Int): Array[Int] =
    val missingSum            = mean * (rolls.length + n) - rolls.sum
    val (quotient, remainder) = missingSum /% n

    if missingSum < n || missingSum > 6 * n then Array.empty
    else Array.tabulate(n)(i => if i < remainder then quotient + 1 else quotient)
