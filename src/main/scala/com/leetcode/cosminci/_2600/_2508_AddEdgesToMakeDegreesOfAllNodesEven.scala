package com.leetcode.cosminci._2600

object _2508_AddEdgesToMakeDegreesOfAllNodesEven:

  def isPossible(n: Int, edges: List[List[Int]]): Boolean =
    val graph = edges.foldLeft(Map.empty[Int, Set[Int]].withDefaultValue(Set.empty)) { (graph, edge) =>
      val (a, b) = (edge.head, edge.last)
      graph.updated(a, graph(a) + b).updated(b, graph(b) + a)
    }
    def f(a: Int, b: Int) = !graph(b).contains(a)

    val odd = (1 to n).filter(i => graph(i).size % 2 == 1)
    if odd.size == 2 then (1 to n).exists(i => f(odd.head, i) && f(odd.last, i))
    else if odd.size == 4 then
      f(odd(0), odd(1)) && f(odd(2), odd(3)) ||
      f(odd(0), odd(2)) && f(odd(1), odd(3)) ||
      f(odd(0), odd(3)) && f(odd(1), odd(2))
    else odd.isEmpty
