package io.github.cosminci.leetcode._2500

import scala.collection.immutable.TreeSet

object _2402_MeetingRoomsIII:

  def mostBooked(n: Int, meetings: Array[Array[Int]]): Int =
    meetings
      .map { case Array(start, end) => (start, end.toLong) }
      .sorted
      .foldLeft(TreeSet.from(0 until n), TreeSet.empty[(Long, Int)], Map.empty[Int, Int].withDefaultValue(0)) {
        case ((available, busy, freq), (start, end)) =>
          val (newAvailable, newBusy) = Iterator
            .iterate((available, busy)) { case (available, busy) => (available + busy.head._2, busy.tail) }
            .dropWhile { case (_, busy) => busy.headOption.exists { case (end, _) => end <= start } }
            .next()
          newAvailable.headOption match
            case Some(room) =>
              (newAvailable - room, newBusy + (end -> room), freq.updated(room, freq(room) + 1))
            case None =>
              val (earliestTs, room) = newBusy.head
              (newAvailable, newBusy.tail + (earliestTs + end - start -> room), freq.updated(room, freq(room) + 1))
      }._3
      .maxBy { case (_, freq) => freq }._1
