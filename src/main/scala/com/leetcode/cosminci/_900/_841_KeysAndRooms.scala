package com.leetcode.cosminci._900

import scala.util.chaining.*

object _841_KeysAndRooms:

  def canVisitAllRooms(rooms: List[List[Int]]): Boolean =
    Iterator
      .iterate((Seq(0), Set(0))) { case (head +: toVisit, visited) =>
        (toVisit ++ rooms(head).filterNot(visited.contains), visited + head)
      }
      .dropWhile { case (toVisit, _) => toVisit.nonEmpty }
      .next()
      .pipe { case (_, visited) => visited.size == rooms.length }
