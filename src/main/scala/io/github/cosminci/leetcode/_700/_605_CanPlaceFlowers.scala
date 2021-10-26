package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _605_CanPlaceFlowers:
  def main(args: Array[String]): Unit =
    println(canPlaceFlowers(Array(0, 0, 0), 0))

  def canPlaceFlowers(flowerBed: Array[Int], n: Int): Boolean =
    var (idx, count) = (0, 0)
    while count < n && idx != flowerBed.length do
      if flowerBed(idx) == 0 then
        val prev = if idx > 0 then flowerBed(idx - 1) else 0
        val next = if idx < flowerBed.length - 1 then flowerBed(idx + 1) else 0

        if prev == 0 && next == 0 then
          flowerBed(idx) = 1
          count += 1
          if count == n then return true
      idx += 1

    count == n

  case class Interval(start: Int, end: Int)
  def canPlaceFlowers2(flowerBed: Array[Int], n: Int): Boolean =
    if n == 0 then return true
    if flowerBed.length == 1 then return flowerBed.head == 0 && n <= 1

    val intervals    = mutable.ListBuffer.empty[Interval]
    var (start, end) = (0, 0)

    def result =
      val i = intervals.foldLeft(0) { case (count, Interval(start, end)) =>
        if start == 0 && end == flowerBed.length then count + (end - start + 1) / 2
        else if start == 0 || end == flowerBed.length then count + (end - start) / 2
        else count + (end - start - 1) / 2
      }
      i >= n

    while true do
      start = flowerBed.indexOf(0, start)
      if start == -1 then return result
      end = start + 1
      while end < flowerBed.length && flowerBed(end) == 0 do end += 1
      intervals.addOne(Interval(start, end))
      start = end

    return false
