package io.github.cosminci.leetcode._600

import scala.collection.mutable

object _547_NumberOfProvinces:
  def main(args: Array[String]): Unit =
    println(
      findCircleNum(
        Array(
          Array(1, 0, 0),
          Array(0, 1, 0),
          Array(0, 0, 1)
        )
      )
    )

  private def findCircleNum(isConnected: Array[Array[Int]]): Int =
    isConnected.indices
      .foldLeft(Set.empty[Int], 0) { case ((visited, count), rootCity) =>
        if visited.contains(rootCity) then (visited, count)
        else {
          val newVisited = mutable.Set.from(visited + rootCity)
          val toVisit    = mutable.Queue(rootCity)
          while toVisit.nonEmpty do
            val city = toVisit.dequeue()
            isConnected(city).indices
              .filter(isConnected(city)(_) == 1)
              .foreach { connectedCity =>
                if !newVisited.contains(connectedCity) then
                  toVisit.enqueue(connectedCity)
                  newVisited.add(connectedCity)
              }
          (newVisited.toSet, count + 1)
        }
      }
      ._2
