package com.leetcode.cosminci._1200

import scala.collection.mutable

object _1146_SnapshotArray:

  def main(args: Array[String]): Unit =
    val arr = new SnapshotArray(4)
    arr.set(2, 3)
    arr.snap()
    arr.set(2, 5)
    arr.snap()
    arr.set(2, 7)
    println(arr.get(2, 0))
    println(arr.get(2, 1))
    println(arr.get(2, 2))

  class SnapshotArray(_length: Int):
    private var snapshotId = 0
    private val array = Array.fill(_length) {
      val ledger = new java.util.TreeMap[Int, Int]()
      ledger.put(snapshotId, 0)
      ledger
    }

    def set(index: Int, `val`: Int) =
      array(index).put(snapshotId, `val`)

    def snap(): Int =
      snapshotId += 1
      snapshotId - 1

    def get(index: Int, snap_id: Int): Int =
      array(index).floorEntry(snap_id).getValue
