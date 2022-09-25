package io.github.cosminci.leetcode._2500

object _2421_NumGoodPaths:

  def numberOfGoodPaths(vals: Array[Int], edges: Array[Array[Int]]): Int =
    val graph = edges.foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) { case (graph, Array(a, b)) =>
      graph.updated(a, graph(a) :+ b).updated(b, graph(b) :+ a)
    }
    val counter = vals.zipWithIndex.foldLeft(Map.empty[Int, Map[Int, Int]]) { case (counter, (v, i)) =>
      counter.updated(i, Map(v -> 1))
    }
    val uf = new UnionFind(vals.length)

    vals.zipWithIndex.sorted
      .foldLeft(vals.length, counter) { case ((res, counter), (v, a)) =>
        graph(a).foldLeft(res, counter) { case ((res, counter), b) =>
          val (rootA, rootB) = (uf.find(a), uf.find(b))
          if vals(b) > v || rootA == rootB then (res, counter)
          else
            uf.parent(rootA) = rootB
            val newCounter = counter
              .updated(rootB, counter(rootB).updated(v, counter(rootB).getOrElse(v, 0) + counter(rootA)(v)))
            val newResult = res + counter(rootA).getOrElse(v, 0) * counter(rootB).getOrElse(v, 0)
            (newResult, newCounter)
        }
      }._1

  class UnionFind(n: Int):
    val parent: Array[Int] = (0 until n).toArray
    def find(a: Int): Int =
      if parent(a) != a then parent(a) = find(parent(a))
      parent(a)
