package io.github.cosminci.leetcode._600

import scala.collection.mutable

object _587_ErectTheFence:

  def main(args: Array[String]): Unit =
    val forest = Array(Array(1, 1), Array(2, 2), Array(2, 0), Array(2, 4), Array(3, 3), Array(4, 2))
    println(outerTrees(forest).map(_.toList).toList)

  private case class Point(x: Int, y: Int)

  def outerTrees(trees: Array[Array[Int]]): Array[Array[Int]] =
    given Ordering[Point] = (p1, p2) => if p1.x == p2.x then p1.y.compare(p2.y) else p1.x.compare(p2.x)
    val points            = trees.map { case Array(x, y) => Point(x, y) }.sorted

    val hull = mutable.Stack.empty[Point]

    // build the lower portion of the hull
    points.indices.foreach { i =>
      while hull.length >= 2 && orientation(hull.tail.head, hull.head, points(i)) > 0 do hull.pop()
      hull.push(points(i))
    }
    hull.pop()

    // build the upper portion of the hull
    points.indices.reverse.foreach { i =>
      while hull.length >= 2 && orientation(hull.tail.head, hull.head, points(i)) > 0 do hull.pop()
      hull.push(points(i))
    }
    hull.distinct.toArray.map(p => Array(p.x, p.y))

  private def orientation(p1: Point, p2: Point, p3: Point): Int =
    (p2.y - p1.y) * (p3.x - p2.x) - (p2.x - p1.x) * (p3.y - p2.y)
