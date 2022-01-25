package io.github.cosminci.leetcode._1000

object _941_ValidMountainArray:
  def main(args: Array[String]): Unit =
    println(validMountainArray(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)))
    println(validMountainArray(Array(3, 5, 5)))
    println(validMountainArray(Array(0, 3, 2, 1)))

  def validMountainArray(arr: Array[Int]): Boolean =
    val peak = Iterator
      .iterate(0)(_ + 1)
      .dropWhile(i => i + 1 < arr.length && arr(i) < arr(i + 1))
      .next()

    peak != 0 && peak != arr.length - 1 && Iterator
      .iterate(peak)(_ + 1)
      .dropWhile(i => i + 1 < arr.length && arr(i) > arr(i + 1))
      .next() == arr.length - 1
