package io.github.cosminci.leetcode._2000

import io.github.cosminci.utils

import scala.collection.mutable

object _1992_FindAllGroupsOfFarmland:
  def main(args: Array[String]): Unit =
    println(
      findFarmland(
        Array(
          Array(1, 0, 0),
          Array(0, 1, 1),
          Array(0, 1, 1)
        )
      ).map(_.toSeq).toSeq
    )

  def findFarmland(land: Array[Array[Int]]): Array[Array[Int]] =
    val (m, n)   = (land.length, land.head.length)
    val farmland = mutable.ListBuffer.empty[Array[Int]]

    def markReachable(r1: Int, c1: Int): Unit =
      var (r2, c2) = (r1, c1)
      while r2 < m && land(r2)(c1) == 1 do r2 += 1
      while c2 < n && land(r1)(c2) == 1 do c2 += 1

      (r1 until r2).foreach { r =>
        (c1 until c2).foreach { c =>
          land(r)(c) = 0
        }
      }
      farmland.append(Array(r1, c1, r2 - 1, c2 - 1))

    land.indices.foreach { r =>
      land(r).indices.foreach { c =>
        if land(r)(c) == 1 then markReachable(r, c)
      }
    }

    farmland.toArray
