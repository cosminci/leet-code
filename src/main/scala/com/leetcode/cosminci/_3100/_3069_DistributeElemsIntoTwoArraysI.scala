package com.leetcode.cosminci._3100

import scala.util.chaining.*

object _3069_DistributeElemsIntoTwoArraysI:

  def resultArray(nums: Array[Int]): Array[Int] =
    nums.drop(2).foldLeft(Array(nums.head), Array(nums.tail.head)) { 
      case ((arr1, arr2), n) =>
        if arr1.last > arr2.last then (arr1 :+ n, arr2) else (arr1, arr2 :+ n)
      }.pipe { case (arr1, arr2) => arr1 ++ arr2 }
