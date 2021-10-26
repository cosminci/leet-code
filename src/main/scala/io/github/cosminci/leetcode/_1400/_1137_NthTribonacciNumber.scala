package io.github.cosminci.leetcode._1400

object _1137_NthTribonacciNumber {
  def main(args: Array[String]): Unit = {
    println(tribonacci(4))
    println(tribonacci(25))
  }

  private def tribonacci(n: Int): Int =
    if (n == 0) 0
    else
      (3 to n).foldLeft(Seq(0, 1, 1)) {
        case (prevThree, _) =>
          prevThree.tail :+ prevThree.sum
      }.last
}
