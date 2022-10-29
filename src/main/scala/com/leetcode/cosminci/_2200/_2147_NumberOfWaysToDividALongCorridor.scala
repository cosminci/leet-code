package com.leetcode.cosminci._2200

import scala.collection.mutable

object _2147_NumberOfWaysToDividALongCorridor:
  def main(args: Array[String]): Unit =
    println(numberOfWays("SSPPSPS"))

  def numberOfWays(corridor: String): Int =
    val seatIndices = corridor.indices.filter(i => corridor(i) == 'S')

    if seatIndices.length % 2 == 1 || seatIndices.isEmpty then 0
    else
      (2 until seatIndices.length by 2)
        .foldLeft(1L, seatIndices(1)) { case ((ways, prev), i) =>
          val length = seatIndices(i) - prev
          ((ways * length) % 1_000_000_007, seatIndices(i + 1))
        }._1.toInt
