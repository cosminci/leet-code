package io.github.cosminci.leetcode._200

object _165_CompareVersionNumbers:
  def main(args: Array[String]): Unit =
    println(compareVersion("1.01", "1.001"))
    println(compareVersion("1.0", "1.0.0"))
    println(compareVersion("0.1", "1.1"))
    println(compareVersion("1.0.1", "1"))
    println(compareVersion("7.5.2.4", "7.5.3"))

  private def compareVersion(version1: String, version2: String): Int =
    version1
      .split("\\.")
      .zipAll(
        version2.split("\\."),
        "0",
        "0"
      )
      .collectFirst {
        case (v1Revision, v2Revision) if v1Revision.toInt != v2Revision.toInt =>
          if v1Revision.toInt > v2Revision.toInt then 1 else -1
      }
      .getOrElse(0)
