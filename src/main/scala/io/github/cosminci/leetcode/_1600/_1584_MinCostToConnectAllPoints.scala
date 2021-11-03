package io.github.cosminci.leetcode._1600

import scala.collection.mutable

object _1584_MinCostToConnectAllPoints:

  def main(args: Array[String]): Unit =
    println(minCostConnectPoints(Array(Array(0, 0), Array(2, 2), Array(3, 10), Array(5, 2), Array(7, 0))))

  def minCostConnectPoints(points: Array[Array[Int]]): Int =
    if points.length == 1 then return 0

    val adjList = mutable.Map.empty[Point, mutable.Set[Edge]]
    points.foreach { case Array(x1, y1) =>
      points.foreach { case Array(x2, y2) =>
        if x1 != x2 || y1 != y2 then
          val edgeWeight = math.abs(x1 - x2) + math.abs(y1 - y2)
          adjList
            .getOrElseUpdate(Point(x1, y1), mutable.Set.empty)
            .add(Edge(Point(x2, y2), edgeWeight))
          adjList
            .getOrElseUpdate(Point(x2, y2), mutable.Set.empty)
            .add(Edge(Point(x1, y1), edgeWeight))
      }
    }
    given Ordering[Edge] = (e1, e2) => e2.weight.compareTo(e1.weight)

    val root    = adjList.keysIterator.next()
    val toVisit = mutable.PriorityQueue.from(adjList(root))
    val visited = mutable.Set(root)

    var cost = 0
    while visited.size != points.length do
      val edge = toVisit.dequeue()
      if !visited.contains(edge.adjacent) then
        visited.add(edge.adjacent)
        toVisit.addAll(adjList(edge.adjacent))
        cost += edge.weight
    cost

  case class Point(x: Int, y: Int)

  case class Edge(adjacent: Point, weight: Int)
