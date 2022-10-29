package com.leetcode.cosminci._600

object _517_SuperWashingMachines:
  def main(args: Array[String]): Unit =
    println(findMinMoves(Array(1, 0, 5)))
    println(findMinMoves(Array(0, 3, 0)))
    println(findMinMoves(Array(0, 2, 0)))
    println(findMinMoves(Array(0, 0, 11, 5)))

  def findMinMoves(machines: Array[Int]): Int =
    val total = machines.sum
    if total % machines.length != 0 then return -1
    val targetAvg = total / machines.length
    machines
      .map(_ - targetAvg)
      .foldLeft((0, 0)) { case ((balance, result), diffToAvg) =>
        val newBalance = balance + diffToAvg
        (newBalance, Array(result, math.abs(newBalance), diffToAvg).max)
      }
      ._2
