package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _332_ReconstructItinerary:
  def main(args: Array[String]): Unit =
    println(findItinerary(List(List("JFK", "KUL"), List("JFK", "NRT"), List("NRT", "JFK"))))

  def findItinerary(tickets: List[List[String]]): List[String] =
    given Ordering[String] = (x, y) => y.compareTo(x)
    val adjMatrix          = mutable.Map.empty[String, mutable.PriorityQueue[String]]
    tickets.foreach { ticket =>
      adjMatrix.getOrElseUpdate(ticket.head, mutable.PriorityQueue.empty).enqueue(ticket(1))
    }

    val itinerary = mutable.ListBuffer.empty[String]
    def visit(curr: String): Unit =
      while adjMatrix.get(curr).exists(_.nonEmpty) do visit(adjMatrix(curr).dequeue())
      itinerary.prepend(curr)

    visit("JFK")
    itinerary.toList
