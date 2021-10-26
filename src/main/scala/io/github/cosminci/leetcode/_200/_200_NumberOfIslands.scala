package io.github.cosminci.leetcode._200

import io.github.cosminci.utils

import scala.collection.mutable
import scala.util.Random

object _200_NumberOfIslands:

  def main(args: Array[String]): Unit =
    println(
      numIslands(
        Array(
          Array('1', '1', '0', '0', '0'),
          Array('1', '1', '0', '0', '0'),
          Array('0', '0', '1', '0', '0'),
          Array('0', '0', '0', '1', '1')
        )
      )
    )

  private def numIslands(grid: Array[Array[Char]]): Int =
    val nodeToSet = mutable.Map.empty[(Int, Int), Int]

    grid.indices.foreach { i =>
      grid(0).indices.foreach { j =>
        if grid(i)(j) == '1' && !nodeToSet.contains((i, j)) then dfsAddToSet(grid, nodeToSet, i, j)
      }
    }
    nodeToSet.groupBy(_._2).size

  private def dfsAddToSet(grid: Array[Array[Char]], nodeToSet: mutable.Map[(Int, Int), Int], i: Int, j: Int): Unit =
    val setId = Random.nextInt(Int.MaxValue)

    val toVisit = mutable.Stack.empty[(Int, Int)]
    toVisit.push((i, j))
    nodeToSet.update((i, j), setId)

    while toVisit.nonEmpty do
      val node = toVisit.pop()
      utils.neighbours(node._1, node._2, grid).foreach { neighbour =>
        if !nodeToSet.contains(neighbour) then
          toVisit.push(neighbour)
          nodeToSet.update(neighbour, setId)
      }
