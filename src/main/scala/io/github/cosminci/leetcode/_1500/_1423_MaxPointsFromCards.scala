package io.github.cosminci.leetcode._1500

object _1423_MaxPointsFromCards:

  def main(args: Array[String]): Unit =
    List(
      (Array(2, 2, 2), 2),
      (Array(9, 7, 7, 9, 7, 7, 9), 7),
      (Array(1, 1000, 1), 1),
      (Array(1, 79, 80, 1, 1, 1, 200, 1), 3),
      (Array(1, 2, 3, 4, 5, 6, 1), 3)
    ).foreach { case (points, k) =>
      println(s"Precomputed sums: ${maxScorePrecomp(points, k)}")
      println(s"Sliding window 1: ${maxScoreSliding1(points, k)}")
      println(s"Sliding window 2: ${maxScoreSliding2(points, k)}")
    }

  private def maxScorePrecomp(cardPoints: Array[Int], k: Int): Int =
    var max       = 0
    val leftSums  = Array.ofDim[Int](k + 1)
    val rightSums = Array.ofDim[Int](k + 1)
    leftSums(0) = 0
    rightSums(0) = 0
    (1 to k).foreach { cardsToTake =>
      leftSums(cardsToTake) = leftSums(cardsToTake - 1) + cardPoints(cardsToTake - 1)
      rightSums(cardsToTake) = rightSums(cardsToTake - 1) + cardPoints(cardPoints.length - cardsToTake)
    }
    (0 to k).foreach { i =>
      max = math.max(max, leftSums(i) + rightSums(k - i))
    }
    max

  private def maxScoreSliding1(cardPoints: Array[Int], k: Int): Int =
    var sum = cardPoints.take(k).sum
    var max = sum
    (0 until k).foreach { idx =>
      sum = sum - cardPoints(k - idx - 1) + cardPoints(cardPoints.length - idx - 1)
      max = math.max(max, sum)
    }
    max

  private def maxScoreSliding2(cardPoints: Array[Int], k: Int): Int =
    var sum   = cardPoints.take(k).sum
    var max   = sum
    var left  = k - 1
    var right = cardPoints.length - 1
    (0 until k).foreach { _ =>
      sum = sum - cardPoints(left) + cardPoints(right)
      max = math.max(max, sum)
      left -= 1
      right -= 1
    }
    max
