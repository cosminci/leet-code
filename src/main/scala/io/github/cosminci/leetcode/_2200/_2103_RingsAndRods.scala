package io.github.cosminci.leetcode._2200

object _2103_RingsAndRods:
  def countPoints(rings: String): Int =
    rings
      .grouped(2)
      .toSeq
      .groupMap(_.last.toInt)(_.head)
      .count { case (ring, colors) =>
        Set('R', 'G', 'B').forall(colors.contains)
      }
