package com.leetcode.cosminci._1000

object _927_ThreeEqualParts:
  def main(args: Array[String]): Unit =
    println(threeEqualParts(Array(1, 0, 1, 1, 0)).toList)

  def threeEqualParts(arr: Array[Int]): Array[Int] =
    val NoSolution = Array(-1, -1)
    val totalCount = arr.count(_ == 1)
    if totalCount % 3 != 0 then return NoSolution
    if totalCount == 0 then return Array(0, arr.length - 1)

    val (initialP1, initialP2, initialP3) = startingPointers(arr, totalCount)

    var (p1, p2, p3) = (initialP1, initialP2, initialP3)
    while p3 < arr.length do
      if arr(p1) != arr(p2) || arr(p1) != arr(p3) || p1 == initialP2 || p2 == initialP3 then return NoSolution
      p1 += 1
      p2 += 1
      p3 += 1
    Array(p1 - 1, p2)

  def startingPointers(arr: Array[Int], totalCount: Int) =
    var p1 = 0
    while arr(p1) != 1 do p1 += 1
    var (p2, count) = (p1, 0)
    while count <= totalCount / 3 do
      count += arr(p2)
      p2 += 1
    var p3 = p2
    p2 -= 1
    while count <= totalCount * 2 / 3 do
      count += arr(p3)
      p3 += 1
    p3 -= 1
    (p1, p2, p3)
