package io.github.cosminci.leetcode._1500

object _1482_MinNumberOfDaysToMakeMBouquets:
  def main(args: Array[String]): Unit =
    println(minDays(Array(1, 10, 3, 10, 2), 3, 1))
    println(minDays(Array(1, 10, 3, 10, 2), 3, 2))
    println(minDays(Array(7, 7, 7, 7, 12, 7, 7), 2, 3))
    println(minDays(Array(1000000000, 1000000000), 1, 1))
    println(minDays(Array(1, 10, 2, 9, 3, 8, 4, 7, 5, 6), 4, 2))

  def minDays(bloomDay: Array[Int], m: Int, k: Int): Int =
    if m * k > bloomDay.length then return -1

    def canMakeBouquets(day: Int) =
      bloomDay.indices
        .foldLeft((0, 0)) { case ((bouquetCount, flowerCount), bloomDayIdx) =>
          if bloomDay(bloomDayIdx) > day then (bouquetCount, 0)
          else if flowerCount + 1 == k then (bouquetCount + 1, 0)
          else (bouquetCount, flowerCount + 1)
        }
        ._1 >= m

    var (l, r) = (bloomDay.min, bloomDay.max)
    while l < r do
      val mid = l + (r - l) / 2
      if canMakeBouquets(mid) then r = mid
      else l = mid + 1

    l
