package io.github.cosminci.leetcode._900

object _875_KokoEatingBananas:
  def main(args: Array[String]): Unit =
    println(minEatingSpeed(Array(3, 6, 7, 11), 8))
    println(minEatingSpeed(Array(30, 11, 23, 4, 20), 5))
    println(minEatingSpeed(Array(30, 11, 23, 4, 40), 5))
    println(minEatingSpeed(Array(30, 11, 23, 4, 20), 6))

  def minEatingSpeed(piles: Array[Int], h: Int): Int =
    @annotation.tailrec
    def binarySearch(l: Int, r: Int): Int =
      if l >= r then r
      else
        val mid = (l + r) / 2
        if piles.map(p => math.ceil(p.toDouble / mid)).sum <= h then binarySearch(l, mid)
        else binarySearch(mid + 1, r)

    binarySearch(l = 1, r = piles.max)
