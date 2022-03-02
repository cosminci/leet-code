package io.github.cosminci.leetcode._2100

object _2065_MaxPathQualityOfAGraph:
  def maximalPathQuality(values: Array[Int], edges: Array[Array[Int]], maxTime: Int): Int = {
    val adjList = edges.foldLeft(Map.empty[Int, Seq[(Int, Int)]].withDefaultValue(Seq.empty)) {
      case (adj, Array(n1, n2, cost)) =>
        adj
          .updated(n1, adj(n1) :+ (n2, cost))
          .updated(n2, adj(n2) :+ (n1, cost))
    }

    def dfs(node: Int, visited: Set[Int], quality: Int, budget: Int): Int =
      adjList(node)
        .collect {
          case (next, cost) if (cost <= budget) =>
            val nextQuality = Option.when(!visited.contains(next))(values(next)).getOrElse(0)
            dfs(next, visited + next, quality + nextQuality, budget - cost)
        }
        .appendedAll(Option.when(node == 0)(quality))
        .maxOption
        .getOrElse(0)

    dfs(node = 0, visited = Set(0), quality = values.head, budget = maxTime)
  }
