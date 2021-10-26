package io.github.cosminci.leetcode._900

import scala.collection.mutable

object _841_KeysAndRooms:

  private def canVisitAllRoomsBFS(rooms: List[List[Int]]): Boolean =
    val toVisit = mutable.Queue(0)
    val visited = mutable.Set(0)

    while toVisit.nonEmpty do
      val room = toVisit.dequeue()
      rooms(room).foreach { key =>
        if !visited.contains(key) then
          visited.add(key)
          toVisit.enqueue(key)
      }

    visited.size == rooms.length
