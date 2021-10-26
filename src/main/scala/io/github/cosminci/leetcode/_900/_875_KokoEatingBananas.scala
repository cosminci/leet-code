package io.github.cosminci.leetcode._900

object _875_KokoEatingBananas:
  def main(args: Array[String]): Unit =
    println(minEatingSpeed(Array(3, 6, 7, 11), 8))
    println(minEatingSpeed(Array(30, 11, 23, 4, 20), 5))
    println(minEatingSpeed(Array(30, 11, 23, 4, 40), 5))
    println(minEatingSpeed(Array(30, 11, 23, 4, 20), 6))

  private def minEatingSpeed(piles: Array[Int], h: Int): Int =
    val minK = math.ceil(piles.sum / h.toDouble).toInt
    val maxK = piles.max

    var (l, r) = (minK, maxK)
    while l <= r do
      val k = (l + r) / 2
      if canConsumeAll(piles, h, k) then
        if l == k then return k
        r = k
      else l = k + 1
    0

  private def canConsumeAll(piles: Array[Int], h: Int, k: Int): Boolean =
    piles.foldLeft(h) { case (hoursLeft, pile) =>
      hoursLeft - math.ceil(pile / k.toDouble).toInt
    } >= 0
