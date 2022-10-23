package io.github.cosminci.leetcode._2500

import io.github.cosminci.utils.*
import scala.util.chaining.*

object _2447_NumSubarratsWithGcdEqualToK:

  def subarrayGCD(nums: Array[Int], k: Int): Int =
    nums.foldLeft(Map.empty[Int, Int], 0) { case ((gcds, res), n) =>
      if n % k != 0 then (Map.empty, res)
      else gcds
        .updated(n, gcds.getOrElse(n, 0) + 1)
        .foldLeft(Map.empty[Int, Int]) { case (gcds, (prevGcd, count)) =>
          gcd(prevGcd, n).pipe(gcd => gcds.updated(gcd, gcds.getOrElse(gcd, 0) + count))
        }
        .pipe { gcds => (gcds, res + gcds.getOrElse(k, 0)) }
    }._2
