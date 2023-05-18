package com.leetcode.cosminci._1600

import scala.util.chaining.*

object _1557_MinNumVerticesToReachAllNodes:

  def findSmallestSetOfVertices(n: Int, edges: List[List[Int]]): List[Int] =
    (0 until n).diff(edges.map(_.last)).toList
