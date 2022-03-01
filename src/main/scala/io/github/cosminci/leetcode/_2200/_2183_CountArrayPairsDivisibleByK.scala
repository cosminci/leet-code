package io.github.cosminci.leetcode._2200

object _2183_CountArrayPairsDivisibleByK:

  def countPairs(nums: Array[Int], k: Int): Long =
    @annotation.tailrec
    def computeGCD(a: Int, b: Int): Int = if b == 0 then a else computeGCD(b, a % b)

    nums
      .foldLeft(Map.empty[Int, Int].withDefaultValue(0), 0L) { case ((gcds, result), n) =>
        val currGcd  = computeGCD(n, k)
        val nextGcds = gcds.updated(currGcd, gcds(currGcd) + 1)
        val nextResult = gcds.foldLeft(result) { case (result, (prevGcd, count)) =>
          if currGcd.toLong * prevGcd % k == 0 then result + count else result
        }
        (nextGcds, nextResult)
      }._2
