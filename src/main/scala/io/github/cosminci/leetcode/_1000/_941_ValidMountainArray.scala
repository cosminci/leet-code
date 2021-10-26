package io.github.cosminci.leetcode._1000

object _941_ValidMountainArray:
  def main(args: Array[String]): Unit =
    println(validMountainArray(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)))
    println(validMountainArray(Array(3, 5, 5)))
    println(validMountainArray(Array(0, 3, 2, 1)))

  private def validMountainArray(arr: Array[Int]): Boolean =
    var idx = 0

    while idx + 1 < arr.length && arr(idx) < arr(idx + 1) do idx += 1

    if idx == 0 || idx == arr.length - 1 then return false

    while idx + 1 < arr.length && arr(idx) > arr(idx + 1) do idx += 1

    idx == arr.length - 1
