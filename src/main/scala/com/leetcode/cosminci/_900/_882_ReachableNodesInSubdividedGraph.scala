package com.leetcode.cosminci._900

import scala.collection.mutable

object _882_ReachableNodesInSubdividedGraph:
  def main(args: Array[String]): Unit =
    println(
      reachableNodes(
        Array(
          Array(0, 3, 8),
          Array(0, 1, 4),
          Array(2, 4, 3),
          Array(1, 2, 0),
          Array(1, 3, 9),
          Array(0, 4, 7),
          Array(3, 4, 9),
          Array(1, 4, 4),
          Array(0, 2, 7),
          Array(2, 3, 1)
        ),
        8,
        5
      )
    )

  case class NextStop(node: Int, moves: Int)

  def reachableNodes(edges: Array[Array[Int]], maxMoves: Int, n: Int): Int =
    val adjMatrix = edges.foldLeft(Map.empty[Int, Seq[NextStop]].withDefaultValue(Seq.empty)) {
      case (acc, Array(from, to, cost)) =>
        acc
          .updated(from, acc(from).appended(NextStop(to, cost)))
          .updated(to, acc(to).appended(NextStop(from, cost)))
    }
    val movesLeftAfterQueued = mutable.Map.empty[Int, Int]
    val edgeSaturation       = mutable.Map.empty[Int, mutable.Map[Int, Int]]
    val visited              = mutable.Set.empty[Int]

    val toVisit =
      given Ordering[NextStop] = (x, y) => x.moves.compare(y.moves)
      mutable.PriorityQueue(NextStop(0, maxMoves))

    var nodesVisited = 0
    while toVisit.nonEmpty do
      val NextStop(node, movesLeft) = toVisit.dequeue()
      if !visited.contains(node) then
        visited.add(node)
        nodesVisited += 1

        adjMatrix(node).foreach { case NextStop(neighbour, movesRequired) =>
          val movesSaturated = edgeSaturation.get(neighbour).flatMap(_.get(node)).getOrElse(0)

          val movesUsable =
            if movesLeft < movesRequired + 1 then math.min(movesLeft, (movesRequired - movesSaturated))
            else
              val movesLeftAfterNeighbour = movesLeft - movesRequired - 1

              if movesLeftAfterQueued.get(neighbour).forall(_ < movesLeftAfterNeighbour) then
                movesLeftAfterQueued.update(neighbour, movesLeftAfterNeighbour)
                toVisit.enqueue(NextStop(neighbour, movesLeftAfterNeighbour))
              movesRequired - movesSaturated

          nodesVisited += movesUsable
          edgeSaturation.getOrElseUpdate(node, mutable.Map.empty).update(neighbour, movesUsable)
        }

    nodesVisited
