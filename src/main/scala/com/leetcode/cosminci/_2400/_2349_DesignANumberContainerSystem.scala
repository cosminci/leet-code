package com.leetcode.cosminci._2400

import scala.collection.mutable

object _2349_DesignANumberContainerSystem:

  class NumberContainers:
    private val idxToNum = mutable.Map.empty[Int, Int]
    private val numToIdx = mutable.Map.empty[Int, mutable.TreeSet[Int]]

    def change(idx: Int, num: Int): Unit =
      idxToNum.get(idx).foreach(prev => numToIdx(prev).remove(idx))
      idxToNum.update(idx, num)
      numToIdx.getOrElseUpdate(num, mutable.TreeSet.empty).add(idx)

    def find(num: Int): Int =
      numToIdx.get(num).flatMap(_.headOption).getOrElse(-1)
