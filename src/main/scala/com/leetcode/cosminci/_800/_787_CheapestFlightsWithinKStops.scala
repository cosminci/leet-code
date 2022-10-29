package com.leetcode.cosminci._800

import scala.collection.mutable

object _787_CheapestFlightsWithinKStops:
  def main(args: Array[String]): Unit =
    println(findCheapestPrice(3, Array(Array(0, 1, 100), Array(1, 2, 100), Array(0, 2, 500)), 0, 2, 0))
    println(findCheapestPriceBellmanFord(3, Array(Array(0, 1, 100), Array(1, 2, 100), Array(0, 2, 500)), 0, 2, 0))

  def findCheapestPriceBellmanFord(n: Int, flights: Array[Array[Int]], src: Int, dst: Int, k: Int): Int =
    var shortestCost = Array.fill[Int](n + 1)(Int.MaxValue)
    shortestCost(src) = 0

    (1 to k + 1).foreach { _ =>
      val newShortestCost = shortestCost.clone()
      flights.foreach { case Array(from, to, cost) =>
        if shortestCost(from) != Int.MaxValue then
          newShortestCost(to) = math.min(newShortestCost(to), shortestCost(from) + cost)
      }
      shortestCost = newShortestCost
    }

    if shortestCost(dst) != Int.MaxValue then shortestCost(dst) else -1

  def findCheapestPrice(n: Int, flights: Array[Array[Int]], src: Int, dst: Int, k: Int): Int =
    val adjacencyList = flights.foldLeft(Map.empty[Int, Seq[(Int, Int)]]) { case (adjList, Array(from, to, cost)) =>
      adjList.updatedWith(from) {
        case None               => Some(Seq((to, cost)))
        case Some(destinations) => Some(destinations.appended((to, cost)))
      }
    }

    case class NextStop(location: Int, stopsBefore: Int, totalCost: Int)
    val toVisit =
      given Ordering[NextStop] = (x, y) => y.totalCost.compare(x.totalCost)
      mutable.PriorityQueue(NextStop(src, -1, 0))

    val lowestCost   = mutable.Map(src -> 0)
    val shortestPath = mutable.Map(src -> 0)
    while toVisit.nonEmpty do
      val currentStop = toVisit.dequeue()
      if currentStop.location == dst then return currentStop.totalCost

      if currentStop.stopsBefore < k then
        adjacencyList.get(currentStop.location).foreach { nextStops =>
          nextStops.foreach { case (location, cost) =>
            val nextStop = NextStop(location, currentStop.stopsBefore + 1, currentStop.totalCost + cost)

            if lowestCost.get(location).forall(nextStop.totalCost < _) then
              lowestCost.update(location, nextStop.totalCost)
              toVisit.enqueue(nextStop)
            else if shortestPath.get(location).forall(nextStop.stopsBefore < _) then toVisit.enqueue(nextStop)

            shortestPath.update(nextStop.location, nextStop.stopsBefore)
          }
        }
    -1
