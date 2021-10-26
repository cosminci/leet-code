package io.github.cosminci.leetcode._2100

object _2011_FinalValueOfVariableAfterPerformingOps {
  private def finalValueAfterOperations(operations: Array[String]): Int =
    operations.foldLeft(0) {
      case (x, op) =>
        if (op(1) == '+') x + 1 else x - 1
    }
}
