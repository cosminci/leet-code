package io.github.cosminci.leetcode._1000

object _926_FlipStringToMonotoneIncreasing:
  def main(args: Array[String]): Unit =
    println(minFlipsMonoIncr("11011"))
    println(minFlipsMonoIncr("000111"))
    println(minFlipsMonoIncr("100111"))
    println(minFlipsMonoIncr("00110"))
    println(minFlipsMonoIncr("010110"))
    println(minFlipsMonoIncr("00011000"))

  def minFlipsMonoIncr(s: String): Int =
    var onesRemaining     = s.count(_ == '1')
    var zeroesRemaining   = s.length - onesRemaining
    var onesAccumulated   = 0

    (Seq(onesRemaining, zeroesRemaining) ++ (0 until s.length).map { i =>
      if s(i) == '1' then onesAccumulated += 1
      else zeroesRemaining -= 1
      onesAccumulated + zeroesRemaining
    }).min
