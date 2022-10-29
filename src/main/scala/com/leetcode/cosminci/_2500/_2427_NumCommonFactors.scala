package com.leetcode.cosminci._2500

object _2427_NumCommonFactors {

  def commonFactors(a: Int, b: Int): Int = {
    @annotation.tailrec
    def gcd(a: Int, b: Int): Int = if (a == 0) b else gcd(b % a, a)

    (1 to gcd(a, b)).count(f => a % f == 0 && b % f == 0)
  }

}
