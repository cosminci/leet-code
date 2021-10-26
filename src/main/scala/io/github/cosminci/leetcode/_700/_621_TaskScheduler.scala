package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _621_TaskScheduler:
  def main(args: Array[String]): Unit =
    println(leastInterval(Array('A', 'A', 'A', 'B', 'B', 'B'), 2))
    println(leastInterval(Array('A', 'B', 'C', 'A', 'B'), 2))
    println(leastInterval(Array('A', 'A', 'A', 'A', 'A', 'A', 'B', 'C', 'D', 'E', 'F', 'G'), 2))
    println(leastInterval(Array('A', 'A', 'A', 'B', 'B', 'B', 'C', 'C', 'C', 'D', 'D', 'E'), 2))

  private def leastInterval(tasks: Array[Char], n: Int): Int =
    if n == 0 then return tasks.length
    val taskFrequencies: Seq[(Char, Int)] = tasks.groupBy(identity).view.mapValues(_.size).toSeq

    val toSchedule =
      given Ordering[(Char, Int)] = (x, y) => x._2.compare(y._2)
      mutable.PriorityQueue.from(taskFrequencies)
    val coolingDown = mutable.Queue.empty[(Char, Int, Int)]

    var currentTime = 0
    while toSchedule.nonEmpty || coolingDown.nonEmpty do
      coolingDown.dequeueWhile(_._3 == currentTime).foreach { case (task, repeats, finishTs) =>
        toSchedule.enqueue((task, repeats))
      }

      if toSchedule.nonEmpty then
        val (task, repeats) = toSchedule.dequeue()
        if repeats > 1 then coolingDown.enqueue((task, repeats - 1, currentTime + n + 1))

      currentTime += 1

    currentTime
