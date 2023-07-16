package com.leetcode.cosminci._2800

object _2768_NumberOfBlackBlocks:

  def countBlackBlocks(m: Int, n: Int, coordinates: Array[Array[Int]]): Array[Long] =
    val cellsPerBlock = coordinates.foldLeft(Map.empty[(Int, Int), Int].withDefaultValue(0)) {
      case (cnt, Array(x, y)) =>
        Seq((x - 1, y - 1), (x - 1, y), (x, y - 1), (x, y)).foldLeft(cnt) { case (cnt, (x, y)) =>
          if x < 0 || y < 0 || x == m - 1 || y == n - 1 then cnt
          else cnt.updated((x, y), cnt((x, y)) + 1)
        }
    }
    val blocksPerCell = cellsPerBlock.values.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)
    Array.tabulate(5) { i =>
      if i == 0 then (m.toLong - 1) * (n - 1) - blocksPerCell.values.sum
      else blocksPerCell(i)
    }
