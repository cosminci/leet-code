package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _605_CanPlaceFlowers:
  def main(args: Array[String]): Unit =
    println(canPlaceFlowers(Array(0, 0, 0), 0))

  def canPlaceFlowers(flowerBed: Array[Int], n: Int): Boolean =
    (1 to flowerBed.length).foldLeft(0 +: flowerBed.toSeq :+ 0, 0) {
      case ((bed, flowers), i) =>
        if (bed(i) == 0 && bed(i - 1) == 0 && bed(i + 1) == 0)
          (bed.updated(i, 1), flowers + 1)
        else
          (bed, flowers)
      }._2 >= n
