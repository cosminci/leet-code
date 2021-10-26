package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _71_SimplifyPath:
  def main(args: Array[String]): Unit =
    println(simplifyPath("/a/./b/../../c/"))
    println(simplifyPath("/home//foo/"))
    println(simplifyPath("/../"))

  private def simplifyPath(path: String): String =
    val pathBuilder = mutable.Stack.empty[String]
    val segments    = path.split('/').filter(_.nonEmpty)

    segments.foreach { segment =>
      if segment == ".." then
        if pathBuilder.nonEmpty then pathBuilder.pop()
      else if segment != "." then pathBuilder.push(segment)
    }

    pathBuilder.popAll().mkString("/").prepended('/')
