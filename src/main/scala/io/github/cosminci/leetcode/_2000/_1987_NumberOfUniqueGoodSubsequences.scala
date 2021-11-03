package io.github.cosminci.leetcode._2000

object _1987_NumberOfUniqueGoodSubsequences:

  def numberOfUniqueGoodSubsequences(binary: String): Int =
    val mod = 1_000_000_007

    val (ones, zeroes) = binary.foldLeft((0L, 0L)) { case ((oneTerminated, zeroTerminated), n) =>
      if n == '0' then
        val newZeroTerminated = (zeroTerminated + oneTerminated) % mod
        (oneTerminated, newZeroTerminated)
      else
        val newOneTerminated = (oneTerminated + zeroTerminated + 1) % mod
        (newOneTerminated, zeroTerminated)
    }

    val hasZero = if binary.contains('0') then 1 else 0
    ((ones + zeroes + hasZero) % mod).toInt
