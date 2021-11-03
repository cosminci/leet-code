package io.github.cosminci.leetcode._200

object _134_GasStation:
  def main(args: Array[String]): Unit =
    println(canCompleteCircuit(Array(6, 1, 4, 3, 5), Array(3, 8, 2, 4, 2)))
    println(canCompleteCircuit(Array(5, 1, 2, 3, 4), Array(4, 4, 1, 5, 1)))
    println(canCompleteCircuit(Array(3, 4, 5, 1, 2), Array(1, 2, 3, 4, 5)))
    println(canCompleteCircuit(Array(1, 2, 3, 4, 5), Array(3, 4, 5, 1, 2)))
    println(canCompleteCircuit(Array(2, 3, 4), Array(3, 4, 3)))

  def canCompleteCircuit(gas: Array[Int], cost: Array[Int]): Int =
    if gas.sum < cost.sum then return -1

    gas.indices
      .foldLeft((0, 0)) { case ((total, start), i) =>
        val newTotal = total + gas(i) - cost(i)
        if newTotal < 0 then (0, i + 1)
        else (newTotal, start)
      }
      ._2
