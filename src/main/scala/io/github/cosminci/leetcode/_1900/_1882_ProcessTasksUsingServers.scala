package io.github.cosminci.leetcode._1900

import scala.collection.mutable

object _1882_ProcessTasksUsingServers:
  def main(args: Array[String]): Unit =
    println(assignTasks(Array(3, 3, 2), Array(1, 2, 3, 2, 1, 2)).toList)

  def assignTasks(servers: Array[Int], tasks: Array[Int]): Array[Int] =
    val result = Array.ofDim[Int](tasks.length)

    val unscheduledTasks = tasks.zipWithIndex.map { case (duration, scheduleTs) =>
      UnscheduledTask(scheduleTs, duration)
    }

    given Ordering[FreeServer] =
      (x: FreeServer, y: FreeServer) =>
        val weightOrdering = y.weight.compareTo(x.weight)
        if weightOrdering != 0 then weightOrdering else y.id.compare(x.id)
    val freeServers = mutable.PriorityQueue.from(servers.zipWithIndex.map { case (weight, idx) =>
      FreeServer(id = idx, weight = weight)
    })

    given Ordering[BusyServer] = (x: BusyServer, y: BusyServer) => y.until.compareTo(x.until)
    val busyServers            = mutable.PriorityQueue.empty[BusyServer]

    var time    = 0
    var taskIdx = 0
    while taskIdx < tasks.length do
      while busyServers.headOption.map(_.until).exists(_ <= time) do
        val server = busyServers.dequeue()
        freeServers.enqueue(FreeServer(server.id, server.weight))

      while freeServers.nonEmpty && unscheduledTasks(taskIdx).scheduleTs <= time do
        val task   = unscheduledTasks(taskIdx)
        val server = freeServers.dequeue()
        busyServers.enqueue(BusyServer(server.id, server.weight, until = time + task.duration))
        result(taskIdx) = server.id
        taskIdx += 1
        if taskIdx == tasks.length then return result

      time = if freeServers.nonEmpty then unscheduledTasks(taskIdx).scheduleTs else busyServers.head.until
    result

  case class UnscheduledTask(scheduleTs: Int, duration: Int)

  case class FreeServer(id: Int, weight: Int)

  case class BusyServer(id: Int, weight: Int, until: Int)
