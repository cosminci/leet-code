package com.leetcode.cosminci._300

object _264_UglyNumberII:
  def main(args: Array[String]): Unit =
    println(nthUglyNumber(10))

  def nthUglyNumber(n: Int): Int =
    val factors = Array(2, 3, 5)

    val ugly = Array.tabulate(n)(i => if i == 0 then 1 else 0)

    (1 until n).foldLeft(Seq(0, 0, 0)) { case (idx, i) =>
      val candidates = idx.map(ugly).zip(factors).map { case (ugly, factor) => ugly * factor }
      ugly(i) = candidates.min
      idx.zip(candidates).map { case (idx, candidate) => if candidate == ugly(i) then idx + 1 else idx }
    }

    ugly.last
