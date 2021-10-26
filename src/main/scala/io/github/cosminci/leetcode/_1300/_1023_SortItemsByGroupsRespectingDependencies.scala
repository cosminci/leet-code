package io.github.cosminci.leetcode._1300

import scala.collection.mutable

object _1023_SortItemsByGroupsRespectingDependencies:
  def main(args: Array[String]): Unit =
    println(
      sortItems(
        8,
        2,
        Array(-1, -1, 1, 0, 0, 1, 0, -1),
        List(List.empty, List(6), List(5), List(6), List(3, 6), List.empty, List.empty, List.empty)
      ).toList
    )

  private def sortItems(n: Int, m: Int, group: Array[Int], beforeItems: List[List[Int]]): Array[Int] =
    var nextGroupId = m
    group.indices.foreach { i =>
      if group(i) == -1 then group(i) = nextGroupId; nextGroupId += 1
    }
    val groups = group.indices.groupBy(i => group(i))

    val nodePredecesors  = mutable.Map.empty[Int, mutable.Set[Int]].withDefaultValue(mutable.Set.empty)
    val nodeSuccessors   = mutable.Map.empty[Int, mutable.Set[Int]].withDefaultValue(mutable.Set.empty)
    val groupPredecesors = mutable.Map.empty[Int, mutable.Set[Int]].withDefaultValue(mutable.Set.empty)
    val groupSuccessors  = mutable.Map.empty[Int, mutable.Set[Int]].withDefaultValue(mutable.Set.empty)

    beforeItems.zipWithIndex.foreach { case (predecessors, successor) =>
      predecessors.foreach { predecessor =>
        if group(successor) == group(predecessor) then
          nodePredecesors.getOrElseUpdate(successor, mutable.Set.empty).add(predecessor)
          nodeSuccessors.getOrElseUpdate(predecessor, mutable.Set.empty).add(successor)
        else
          groupPredecesors.getOrElseUpdate(group(successor), mutable.Set.empty).add(group(predecessor))
          groupSuccessors.getOrElseUpdate(group(predecessor), mutable.Set.empty).add(group(successor))
      }
    }

    val finalOrder = topologicalSort(groups.keys.toSeq, groupPredecesors, groupSuccessors).flatMap { groupId =>
      val groupNodes           = groups(groupId)
      val nodeOrderWithinGroup = topologicalSort(groupNodes.toSeq, nodePredecesors, nodeSuccessors)
      if nodeOrderWithinGroup.isEmpty then return Array.empty
      nodeOrderWithinGroup
    }

    if finalOrder.length == n then finalOrder.toArray else Array.empty

  private def topologicalSort(
      items: Seq[Int],
      predecessors: mutable.Map[Int, mutable.Set[Int]],
      successors: mutable.Map[Int, mutable.Set[Int]]
  ): Seq[Int] =
    val results = mutable.ListBuffer.empty[Int]
    val toVisit = mutable.Queue.from(items.filter(i => predecessors(i).isEmpty))
    while toVisit.nonEmpty do
      val next = toVisit.dequeue()
      results.addOne(next)
      successors(next).foreach { successor =>
        predecessors(successor).remove(next)
        if predecessors(successor).isEmpty then toVisit.enqueue(successor)
      }
    if results.length == items.length then results.toSeq else Seq.empty
