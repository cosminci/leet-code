package io.github.cosminci.leetcode._900

object _878_NthMagicalNumber:
  def nthMagicalNumber(n: Int, a: Int, b: Int): Int =
    @annotation.tailrec
    def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)

    val lcm = a * b / gcd(a, b)
    @annotation.tailrec
    def binarySearch(l: Long, r: Long): Int =
      if l >= r then (l % 1_000_000_007).toInt
      else
        val mid = l + (r - l) / 2
        if mid / a + mid / b - mid / lcm < n then binarySearch(mid + 1, r)
        else binarySearch(l, mid)
    binarySearch(l = 2L, r = n.toLong * a.min(b))
