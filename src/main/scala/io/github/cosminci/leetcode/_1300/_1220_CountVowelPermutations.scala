package io.github.cosminci.leetcode._1300

object _1220_CountVowelPermutations:

  private val div                = (math.pow(10, 9) + 7).toLong

  def main(args: Array[String]): Unit =
    println(countVowelPermutation(20000))

  def countVowelPermutation(n: Int): Int =
    val dp = Array.ofDim[Long](n, 5)
    (0 until 5).foreach(i => dp(0)(i) = 1)
    (1 until n).foreach { i =>
      val prevCounts = dp(i - 1)
      dp(i)(0) = mod(prevCounts(1) + prevCounts(2) + prevCounts(4))
      dp(i)(1) = mod(prevCounts(0) + prevCounts(2))
      dp(i)(2) = mod(prevCounts(1) + prevCounts(3))
      dp(i)(3) = mod(prevCounts(2))
      dp(i)(4) = mod(prevCounts(2) + prevCounts(3))
    }
    mod(dp.last.sum).toInt

  def mod(n: Long): Long = n % div
