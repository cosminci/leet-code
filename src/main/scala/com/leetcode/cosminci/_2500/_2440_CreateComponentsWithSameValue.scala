package com.leetcode.cosminci._2500

import scala.collection.mutable

object _2440_CreateComponentsWithSameValue:

  def componentValue(nums: Array[Int], edges: Array[Array[Int]]): Int =
    val (sum, n) = (nums.sum, nums.length)
    if n == 1 then return 0

    val graph = mutable.Map.empty[Int, mutable.ListBuffer[Int]]
    val deg   = Array.fill(n)(0)
    edges.foreach { case Array(a, b) =>
      graph.getOrElseUpdate(a, mutable.ListBuffer.empty).append(b)
      graph.getOrElseUpdate(b, mutable.ListBuffer.empty).append(a)
      deg(a) += 1
      deg(b) += 1
    }

    def bfs(target: Int): Boolean =
      val values  = Array.from(nums)
      val degree  = Array.from(deg)
      val toVisit = mutable.Queue.from(nums.indices.filter(n => degree(n) == 1))

      while toVisit.nonEmpty do
        val curr = toVisit.dequeue()
        if degree(curr) != 0 then
          degree(curr) = 0
          if values(curr) == target then
            graph(curr).foreach { next =>
              degree(next) -= 1
              if degree(next) == 0 then return values(next) == target
              else if degree(next) == 1 then toVisit.enqueue(next)
            }
          else
            graph(curr).foreach { next =>
              if degree(next) > 0 then
                degree(next) -= 1
                values(next) += values(curr)
                if degree(next) == 0 then return values(next) == target
                else if degree(next) == 1 then toVisit.enqueue(next)
            }
      false

    (1 until sum)
      .collectFirst {
        case candidate if sum % candidate == 0 && bfs(candidate) => sum / candidate - 1
      }
      .getOrElse(0)
