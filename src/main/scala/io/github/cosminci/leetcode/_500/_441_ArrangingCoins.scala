package io.github.cosminci.leetcode._500

object _441_ArrangingCoins:
  def main(args: Array[String]): Unit =
    (1 to 7).foreach(coins => println(arrangeCoinsBinarySearch(coins)))
    (1 to 7).foreach(coins => println(arrangeCoinsCompletingTheSquare(coins)))
    println(arrangeCoinsBinarySearch(1804289383))
    println(arrangeCoinsCompletingTheSquare(1804289383))

  private def arrangeCoinsBinarySearch(n: Int): Int =
    var (l, r) = (0, n)
    while l <= r do
      val mid  = l + (r - l) / 2
      val rows = mid.toLong * (mid + 1) / 2
      if rows == n then return mid
      else if rows < n then l = mid + 1
      else r = mid - 1
    l - 1

  private def arrangeCoinsCompletingTheSquare(n: Int): Int =
    (math.sqrt(2 * n.toLong + 0.25) - 0.5).toInt
