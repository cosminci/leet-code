package io.github.cosminci.leetcode._400

object _388_LongestAbsoluteFilePath {
  def main(args: Array[String]): Unit = {
    println(lengthLongestPath("dir\n\tsubdir1\n\tsubdir2\n\t\tfile.ext"))
  }

  def lengthLongestPath(input: String): Int =
    input.split('\n').foldLeft(Map(-1 -> 0), 0) {
      case ((pathLengths, maxLength), line) =>
        val depth = line.count(_ == '\t')
        val length = pathLengths(depth - 1) + line.length - depth
        val newMaxLength = if (line.contains('.')) math.max(maxLength, length + depth) else maxLength
        (pathLengths.updated(depth, length), newMaxLength)
    }._2
}
