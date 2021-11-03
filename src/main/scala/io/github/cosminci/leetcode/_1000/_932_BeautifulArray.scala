package io.github.cosminci.leetcode._1000

object _932_BeautifulArray:
  def main(args: Array[String]): Unit =
    println(beautifulArray(5).toList)

  def beautifulArray(n: Int): Array[Int] =
    var result = Seq(1)
    (2 to n).foreach { iteration =>
      result = (result.map(_ * 2 - 1) ++ result.map(_ * 2)).filter(_ <= iteration)
    }
    result.toArray
