package com.leetcode.cosminci._1000

import scala.collection.mutable

object _925_LongPressedName:
  def main(args: Array[String]): Unit =
    println(isLongPressedName("vtkgn", "vttkgnx"))
    println(isLongPressedName("vtkgn", "vttkgnn"))

  def isLongPressedName(name: String, typed: String): Boolean =
    var idx        = 0
    var prevPopped = '_'
    typed.foreach { char =>
      if idx == name.length then
        if char != prevPopped then return false
      else if char == name(idx) then
        idx += 1
        prevPopped = char
      else if prevPopped != char then return false
    }
    idx == name.length
