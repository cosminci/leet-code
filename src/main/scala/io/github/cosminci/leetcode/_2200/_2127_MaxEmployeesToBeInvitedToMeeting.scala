package io.github.cosminci.leetcode._2200

object _2127_MaxEmployeesToBeInvitedToMeeting {
  def maximumInvitations(favorite: Array[Int]): Int = {
    val n = favorite.length
    val (indegree, dp) = (Array.fill(n)(0), Array.fill(n)(0))
    favorite.foreach(fav => indegree(fav) += 1)
    val queue = (0 until n).filter(i => indegree(i) == 0)
    val visited = Array.tabulate(n)(i => Option.when(indegree(i) == 0)(1).getOrElse(0))

    @annotation.tailrec
    def topologicalSort(queue: Seq[Int]): Unit =
      if (queue.isEmpty) return
      val fav = favorite(queue.head)
      dp(fav) = dp(fav).max(dp(queue.head) + 1)
      indegree(fav) -= 1
      if (indegree(fav) > 0) topologicalSort(queue.tail)
      else {
        visited(fav) = 1
        topologicalSort(queue.tail :+ fav)
      }

    topologicalSort(queue)

    @annotation.tailrec
    def maxLength(person: Int, length: Int): Int =
      if (visited(person) == 1) length
      else {
        visited(person) = 1
        maxLength(favorite(person), length + 1)
      }

    val (type1, type2) = favorite.zipWithIndex.foldLeft(0, 0) {
      case ((type1, type2), (fav, person)) =>
        if (visited(person) == 1) (type1, type2)
        else {
          val length = maxLength(person, length = 0)
          if (length == 2) (type1 + dp(person) + dp(fav) + 2, type2)
          else (type1, type2.max(length))
        }
    }
    type1.max(type2)
  }
}
