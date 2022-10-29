package com.leetcode.cosminci._400

import scala.collection.mutable

object _331_VerifyPreorderSerializationOfABinaryTree:
  def main(args: Array[String]): Unit =
    println(isValidSerialization("9,3,4,#,#,1,#,#,2,#,6,#,#"))
    println(isValidSerialization("9,3,4,#,#,1,#,#,#,2,#,6,#,#"))

  def isValidSerialization(preorder: String): Boolean =
    var emptySlots = 1

    preorder.split(',').foreach { n =>
      if (emptySlots == 0) return false
      n match {
        case "#" => emptySlots -= 1
        case _ => emptySlots += 1
      }
    }

    emptySlots == 0
