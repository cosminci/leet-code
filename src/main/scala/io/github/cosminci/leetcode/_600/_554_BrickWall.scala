package io.github.cosminci.leetcode._600

import scala.collection.mutable

object _554_BrickWall:
  def main(args: Array[String]): Unit =
    println(
      leastBricks(
        List(
          List(1, 2, 2, 1),
          List(3, 1, 2),
          List(1, 3, 2),
          List(2, 4),
          List(3, 1, 2),
          List(1, 3, 1, 1)
        )
      )
    )
    println(leastBricks(List(List(1), List(1), List(1))))

  def leastBricks(wall: List[List[Int]]): Int =
    val edgeCounts = mutable.Map.empty[Int, Int]
    val wallWidth  = wall.head.sum
    val wallHeight = wall.length
    wall.foreach { bricks =>
      var idx = 0
      bricks.foreach { length =>
        edgeCounts.updateWith(idx) {
          case None    => Some(1)
          case Some(c) => Some(c + 1)
        }
        edgeCounts.updateWith(idx + length) {
          case None    => Some(1)
          case Some(c) => Some(c + 1)
        }
        idx += length
      }
    }
    val maxEdges = edgeCounts
      .collect {
        case (edge, count) if edge != 0 && edge != wallWidth => count / 2
      }
      .maxOption
      .getOrElse(0)

    wallHeight - maxEdges
