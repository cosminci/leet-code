package io.github.cosminci.leetcode._100

object _69_SqrtX:
  def main(args: Array[String]): Unit =
    println(mySqrt(2147395599))

  private def mySqrt(x: Int): Int =
    if x == 0 then return 0
    if x == 1 then return 1
    var (l, r) = (0, x)
    while l < r do
      val mid = l + (r - l) / 2
      if x / mid >= mid then l = mid + 1
      else r = mid
    l - 1
