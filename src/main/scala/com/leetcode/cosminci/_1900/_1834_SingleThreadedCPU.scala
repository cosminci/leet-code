package com.leetcode.cosminci._1900

import com.leetcode.cosminci.utils

import scala.collection.mutable

object _1834_SingleThreadedCPU:

  def getOrder(tasks: Array[Array[Int]]): Array[Int] =
    val toSchedule = mutable.PriorityQueue.from(tasks.indices.map { i =>
      val Array(et, pt) = tasks(i)
      UnscheduledTask(i, et, pt)
    })

    var timestamp                   = 0
    var busy: Option[ExecutingTask] = None
    val results                     = mutable.ListBuffer.empty[Int]

    val scheduled = mutable.PriorityQueue.empty[ScheduledTask]
    while results.size != tasks.length do
      while toSchedule.headOption.exists(_.enqueueTime <= timestamp) do
        val task = toSchedule.dequeue()
        scheduled.enqueue(ScheduledTask(task.id, task.processingTime))
      busy.foreach { case ExecutingTask(id, finishTime) =>
        if finishTime == timestamp then
          results.addOne(id)
          busy = None
      }
      if busy.isEmpty && scheduled.nonEmpty then
        val task = scheduled.dequeue()
        busy = Some(ExecutingTask(task.id, timestamp + task.processingTime))
      val currentTaskFinishTs = busy.map(_.finishTime).getOrElse(Int.MaxValue)
      val nextTaskScheduleTs  = toSchedule.headOption.map(_.enqueueTime).getOrElse(Int.MaxValue)
      timestamp = math.min(currentTaskFinishTs, nextTaskScheduleTs)

    results.toArray

  case class UnscheduledTask(id: Int, enqueueTime: Int, processingTime: Int)

  case class ScheduledTask(id: Int, processingTime: Int)

  given Ordering[UnscheduledTask] = (x: UnscheduledTask, y: UnscheduledTask) => y.enqueueTime.compare(x.enqueueTime)

  given Ordering[ScheduledTask] = (x: ScheduledTask, y: ScheduledTask) =>
    val etComparison = y.processingTime.compare(x.processingTime)
    if etComparison != 0 then etComparison else y.id.compare(x.id)

  case class ExecutingTask(id: Int, finishTime: Int)
