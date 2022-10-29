package com.leetcode.cosminci._900

object _837_New21Game:
  def main(args: Array[String]): Unit =
    println(new21Game(21, 17, 10))

  def new21Game(n: Int, k: Int, w: Int): Double = {
    if (k == 0 || n >= k + w) return 1.0

    val probabilities = 1.0 +: Array.fill(n)(0.0)

    (1 to n).foldLeft(1.0) { case (wSum, i) =>
      probabilities.update(i, wSum / w)
      wSum +
        Option.when(i < k)(probabilities(i)).getOrElse(0.0) -
        Option.when(i - w >= 0)(probabilities(i - w)).getOrElse(0.0)
    }

    probabilities.slice(k, probabilities.length).sum
  }
