package io.github.cosminci.leetcode._600

object _593_ValidSquare:
  def main(args: Array[String]): Unit =
    println(validSquare(Array(1, 0), Array(-1, 0), Array(0, 1), Array(0, -1)))

  private def validSquare(p1: Array[Int], p2: Array[Int], p3: Array[Int], p4: Array[Int]): Boolean =
    def distance(x: Array[Int], y: Array[Int]) =
      (x(0) - y(0)) * (x(0) - y(0)) + (x(1) - y(1)) * (x(1) - y(1))

    val distanceSet = Set.from(Seq(p1, p2, p3, p4).combinations(2).map { case Seq(x, y) => distance(x, y) })

    !distanceSet.contains(0) && distanceSet.size == 2
