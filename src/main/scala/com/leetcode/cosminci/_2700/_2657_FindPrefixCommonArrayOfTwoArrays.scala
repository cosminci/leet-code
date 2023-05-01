package com.leetcode.cosminci._2700

import scala.collection.immutable.BitSet
import scala.util.chaining.*

object _2657_FindPrefixCommonArrayOfTwoArrays:

  def findThePrefixCommonArray(A: Array[Int], B: Array[Int]): Array[Int] =
    A.zip(B)
      .foldLeft(Array.empty[Int], BitSet.empty, BitSet.empty) { case ((res, setA, setB), (a, b)) =>
        (setA + a, setB + b).pipe { case (setA, setB) =>
          (res :+ setA.intersect(setB).size, setA, setB)
        }
      }.pipe { case (res, _, _) => res }
