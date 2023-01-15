package com.leetcode.cosminci._2600

import scala.collection.MapView

object _2536_IncrementSubmatricesByOne:

  def rangeAddQueries(n: Int, queries: Array[Array[Int]]): Array[Array[Int]] =
    val rowChanges = queries.view
      .flatMap { case Array(r1, c1, r2, c2) => (r1 to r2).flatMap(r => Seq((r, c1, 1), (r, c2 + 1, -1))) }
      .groupMap { case (r, _, _) => r } { case (_, c, delta) => (c, delta) }
      .view.mapValues(_.groupMapReduce(_._1)(_._2)(_ + _).withDefaultValue(0))
      .toMap.withDefaultValue(Map.empty.withDefaultValue(0))

    (0 until n).map { r =>
      (0 until n).scanLeft(0)((acc, c) => acc + rowChanges(r)(c)).tail.toArray
    }.toArray
