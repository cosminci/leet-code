package com.leetcode.cosminci._1000

object _975_OddEvenJump:
  def main(args: Array[String]): Unit =
    println(oddEvenJumps(Array(10, 13, 12, 14, 15)))

  def oddEvenJumps(arr: Array[Int]): Int =
    val canOddJumpFrom = Array.ofDim[Boolean](arr.length)
    canOddJumpFrom(canOddJumpFrom.length - 1) = true
    val canEvenJumpFrom = Array.ofDim[Boolean](arr.length)
    canEvenJumpFrom(canEvenJumpFrom.length - 1) = true

    val nextPositions = new java.util.TreeMap[Int, Int]()
    nextPositions.put(arr.last, arr.length - 1)

    (arr.length - 2 to 0 by -1).foreach { idx =>
      Option(nextPositions.ceilingKey(arr(idx))).foreach { minHigher =>
        canOddJumpFrom(idx) = canEvenJumpFrom(nextPositions.get(minHigher))
      }
      Option(nextPositions.floorKey(arr(idx))).foreach { maxLower =>
        canEvenJumpFrom(idx) = canOddJumpFrom(nextPositions.get(maxLower))
      }
      nextPositions.put(arr(idx), idx)
    }

    canOddJumpFrom.count(_ == true)
