package com.leetcode.cosminci._3000

object _2961_DoubleModularExponentiation:

  def getGoodIndices(variables: Array[Array[Int]], target: Int): List[Int] =
    def modpow(base: Long, pow: Long, mod: Int): Long =
      if pow == 0 then 1
      else
        val a = modpow(base, pow / 2, mod)
        val b = (a * a) % mod
        if pow % 2 == 0 then b else (b * base) % mod

    variables.zipWithIndex.collect {
      case (Array(a, b, c, m), i) if modpow(modpow(a, b, 10), c, m) == target => i
    }.toList
