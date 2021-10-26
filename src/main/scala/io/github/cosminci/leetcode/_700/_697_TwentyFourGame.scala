package io.github.cosminci.leetcode._700

object _697_TwentyFourGame:
  def main(args: Array[String]): Unit =
    println(judgePoint24(Array(4, 0, 8, 7)))

  private def judgePoint24(cards: Array[Int]): Boolean =
    judgePoint24(cards.map(_.toDouble).toList)

  private def judgePoint24(cards: List[Double]): Boolean =
    cards.permutations.exists {
      case _ :: Nil =>
        math.abs(cards.head - 24) < 0.01
      case a :: b :: rest =>
        List(a + b, a - b, a * b, if b != 0 then a / b else 0)
          .exists(folded => judgePoint24(folded +: rest))
      case _ => false // provided for pattern matching completeness
    }
