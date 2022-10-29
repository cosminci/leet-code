package com.leetcode.cosminci._2100

import scala.collection.mutable

object _2039_TheTimeWhenTheNetworkBecomesIdle:
  def main(args: Array[String]): Unit =
    println(networkBecomesIdle(Array(Array(0, 1), Array(1, 2)), Array(0, 2, 1)))

  def networkBecomesIdle(edges: Array[Array[Int]], patience: Array[Int]): Int =
    val adjMatrix = mutable.Map.empty[Int, mutable.ListBuffer[Int]]
    edges.foreach { case Array(from, to) =>
      adjMatrix.getOrElseUpdate(from, mutable.ListBuffer.empty).append(to)
      adjMatrix.getOrElseUpdate(to, mutable.ListBuffer.empty).append(from)
    }

    var maxDuration = 0
    val toVisit     = mutable.Queue.from(adjMatrix(0).map(n => (n, 1)))
    val visited     = mutable.Set.from(adjMatrix(0) :+ 0)

    while toVisit.nonEmpty do
      val (curr, distance)  = toVisit.dequeue()
      val lastRetryTimeLeft = (2 * distance - 1) / patience(curr) * patience(curr)
      maxDuration = math.max(maxDuration, distance * 2 + lastRetryTimeLeft)

      adjMatrix(curr).foreach { next =>
        if !visited.contains(next) then
          visited.add(next)
          toVisit.enqueue((next, distance + 1))
      }

    maxDuration + 1
