package com.leetcode.cosminci._2400

object _2392_BuildMatrixWithConditions:

  def buildMatrix(k: Int, rowConditions: Array[Array[Int]], colConditions: Array[Array[Int]]): Array[Array[Int]] =
    def topologicalSort(edges: Array[Seq[Int]]): Option[Seq[Int]] =
      val (graph, indegree) =
        edges.foldLeft(Map.empty[Int, Set[Int]].withDefaultValue(Set.empty), Map.empty[Int, Int].withDefaultValue(0)) {
          case ((graph, indegree), Seq(before, after)) =>
            (graph.updated(before, graph(before) + after), indegree.updated(after, indegree(after) + 1))
        }

      def dfs(toVisit: Seq[Int], indegree: Map[Int, Int]): Seq[Int] =
        toVisit.headOption match
          case None => Seq.empty
          case Some(curr) =>
            curr +: dfs.tupled(graph(curr).foldLeft(toVisit.tail, indegree) { case ((toVisit, indegree), next) =>
              if indegree(next) == 1 then (toVisit :+ next, indegree.removed(next))
              else (toVisit, indegree.updated(next, indegree(next) - 1))
            })

      Option(dfs(toVisit = (1 to k).filter(n => indegree(n) == 0), indegree)).filter(_.length == k)

    val maybeResult = for
      order1 <- topologicalSort(rowConditions.map(_.toSeq).distinct)
      order2 <- topologicalSort(colConditions.map(_.toSeq).distinct)
    yield Array.tabulate(k, k) { case (r, c) => if order1(r) == order2(c) then order1(r) else 0 }

    maybeResult.getOrElse(Array.empty)
