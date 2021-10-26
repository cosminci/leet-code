package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _397_IntegerReplacement {
  def main(args: Array[String]): Unit = {
    println(integerReplacementBFS(Int.MaxValue))
    println(integerReplacementDFS(Int.MaxValue))
    println(integerReplacementBFS(7))
    println(integerReplacementDFS(7))
  }

  private def integerReplacementDFS(num: Int) = {
    def dfs(n: Long): Int =
      if (n == 1) 0
      else if (n % 2 == 0) 1 + dfs(n / 2)
      else 1 + math.min(dfs(n - 1), dfs(n + 1))

    dfs(num)
  }

  private def integerReplacementBFS(n: Int): Int = {
    val toVisit = mutable.Queue((n.toLong, 0))
    val visited = mutable.Set(n.toLong)

    while (toVisit.nonEmpty) {
      toVisit.dequeueAll(_ => true).foreach { case (curr, steps) =>
        if (curr == 1) return steps

        val neighbours = if (curr % 2 == 0) Seq(curr / 2) else Seq(curr - 1, curr + 1)
        neighbours.filterNot(visited.contains).foreach { neighbour =>
          toVisit.enqueue((neighbour, steps + 1))
          visited.add(neighbour)
        }
      }
    }

    Int.MaxValue
  }
}
