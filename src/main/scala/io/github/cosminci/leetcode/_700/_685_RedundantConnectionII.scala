package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _685_RedundantConnectionII:

  def main(args: Array[String]): Unit =
    println(
      findRedundantDirectedConnection(Array(Array(1, 3), Array(1, 4), Array(5, 1), Array(5, 2), Array(5, 3))).toList
    )
    println(findRedundantDirectedConnection(Array(Array(2, 1), Array(3, 1), Array(4, 2), Array(1, 4))).toList)
    println(findRedundantDirectedConnection(Array(Array(1, 2), Array(1, 3), Array(2, 3))).toList)

  def findRedundantDirectedConnection(edges: Array[Array[Int]]): Array[Int] =
    val inEdges   = mutable.Map.empty[Int, mutable.Set[Int]]
    val outEdges  = mutable.Map.empty[Int, mutable.Set[Int]]
    val nodes     = mutable.Set.empty[Int]
    val edgeOrder = mutable.Map.empty[(Int, Int), Int]

    edges.indices.foreach { i =>
      val Array(from, to) = edges(i)
      inEdges.getOrElseUpdate(to, mutable.Set.empty).addOne(from)
      outEdges.getOrElseUpdate(from, mutable.Set.empty).addOne(to)
      nodes.add(from)
      nodes.add(to)
      edgeOrder.update((from, to), i)
    }

    inEdges.find { case (_, parents) => parents.size == 2 } match
      case None =>
        // cycle present - find first edge that leads back to an existing node
        val parentNodes = mutable.Set.empty[Int]
        edges.foreach { case edge @ Array(from, to) =>
          if parentNodes.contains(to) then return edge
          parentNodes.add(from)
        }
        Array.empty
      case Some((node, children)) =>
        // two candidate edges - remove the higher index one and bfs from the root to see if all nodes are reachable
        val (candidate1, candidate2) =
          if edgeOrder(children.head, node) < edgeOrder(children.last, node) then
            (Array(children.last, node), Array(children.head, node))
          else (Array(children.head, node), Array(children.last, node))

        outEdges(candidate1.head).remove(candidate1.last)

        val root = nodes.find(n => !inEdges.contains(n)).get

        val visited = mutable.Set.empty[Int]
        val toVisit = mutable.Stack.empty[Int]
        toVisit.addOne(root)

        while toVisit.nonEmpty do
          val node = toVisit.pop()
          visited.add(node)
          if outEdges.contains(node) then outEdges(node).foreach(toVisit.push)

        if visited.size == nodes.size then candidate1 else candidate2
