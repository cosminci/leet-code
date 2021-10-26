package io.github.cosminci.leetcode._1000

object _935_KnightDialer:
  def main(args: Array[String]): Unit =
    println(knightDialer(4))

  private def knightDialer(n: Int): Int =
    val sources = Map(
      0 -> Seq(4, 6),
      1 -> Seq(6, 8),
      2 -> Seq(7, 9),
      3 -> Seq(4, 8),
      4 -> Seq(0, 3, 9),
      5 -> Seq.empty,
      6 -> Seq(0, 1, 7),
      7 -> Seq(2, 6),
      8 -> Seq(1, 3),
      9 -> Seq(2, 4)
    )
    val mod = 1_000_000_007

    var counts = Array.fill[Long](10)(1)
    (2 to n).foreach { _ =>
      val newCounts = Array.ofDim[Long](10)
      sources.foreach { case (d, s) =>
        newCounts(d) = s.map(counts).sum % mod
      }
      counts = newCounts
    }

    (counts.sum % mod).toInt
