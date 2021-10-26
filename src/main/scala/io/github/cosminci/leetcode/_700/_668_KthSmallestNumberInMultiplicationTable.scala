package io.github.cosminci.leetcode._700

object _668_KthSmallestNumberInMultiplicationTable:
  def main(args: Array[String]): Unit =
    println(findKthNumber(3, 3, 5))
    println(findKthNumber(2, 3, 6))

  private def findKthNumber(m: Int, n: Int, k: Int): Int =
    def enoughSmallerNumbers(value: Int) =
      val range = (1 to m).takeWhile(row => math.min(value / row, n) > 0)
      val count = range.foldLeft(0) { case (acc, row) =>
        acc + math.min(value / row, n)
      }
      count >= k

    var (l, r) = (1, m * n)
    while l < r do
      val mid = l + (r - l) / 2
      if enoughSmallerNumbers(mid) then r = mid
      else l = mid + 1
    l
