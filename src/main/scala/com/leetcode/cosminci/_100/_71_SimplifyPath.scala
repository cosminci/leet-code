package com.leetcode.cosminci._100

import scala.collection.mutable

object _71_SimplifyPath:
  def main(args: Array[String]): Unit =
    println(simplifyPath("/a/./b/../../c/"))
    println(simplifyPath("/home//foo/"))
    println(simplifyPath("/../"))

  def simplifyPath(path: String): String =
    path
      .split('/')
      .filter(_.nonEmpty)
      .foldLeft(Seq.empty[String]) { case (acc, segment) =>
        if segment == ".." then acc.dropRight(1)
        else if segment != "." then acc :+ segment
        else acc
      }
      .mkString("/")
      .prepended('/')
