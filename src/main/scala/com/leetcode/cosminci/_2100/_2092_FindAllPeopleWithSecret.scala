package com.leetcode.cosminci._2100

import com.leetcode.cosminci.utils.UnionFind

import scala.collection.immutable.TreeMap

object _2092_FindAllPeopleWithSecret:
  def main(args: Array[String]): Unit =
    println(findAllPeople(6, Array(Array(0, 2, 1), Array(1, 3, 1), Array(4, 5, 1)), 1))

  def findAllPeople(n: Int, meetings: Array[Array[Int]], firstPerson: Int): List[Int] =
    TreeMap
      .from(meetings.groupBy(_.last))
      .foldLeft(Set(0, firstPerson)) { case (secretHolders, (time, meetings)) =>
        val uf = new UnionFind[Int]
        meetings.foreach { case Array(p1, p2, _) => uf.union(p1, p2) }
        secretHolders ++ uf
          .nodes()
          .groupBy(uf.find)
          .map { case (k, values) => values + k }
          .filter(_.intersect(secretHolders).size > 0)
          .flatten
      }
      .toList
