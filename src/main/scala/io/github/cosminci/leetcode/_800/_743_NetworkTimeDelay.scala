package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _743_NetworkTimeDelay:
  def main(args: Array[String]): Unit =
    println(networkDelayTime(Array(Array(1, 2, 1), Array(2, 3, 2), Array(1, 3, 4)), 3, 1))
  def networkDelayTime(times: Array[Array[Int]], n: Int, k: Int): Int =
    val adjList = mutable.Map.empty[Int, mutable.Map[Int, Int]]
    (1 to n).foreach { node =>
      adjList.update(node, mutable.Map.empty)
    }
    times.foreach { case Array(from, to, weight) =>
      adjList(from).update(to, weight)
    }
    given Ordering[(Int, Int)] = (x, y) => y._2.compareTo(x._2)

    val pqueue  = mutable.PriorityQueue((k, 0))
    val visited = mutable.Set.empty[Int]
    var max     = 0
    while pqueue.nonEmpty do
      val (node, timeSoFar) = pqueue.dequeue()
      if !visited.contains(node) then
        visited.add(node)
        max = math.max(max, timeSoFar)
        adjList(node).foreach { case (neighbour, timeToReach) =>
          pqueue.enqueue((neighbour, timeSoFar + timeToReach))
        }
    if visited.size == n then max else -1
