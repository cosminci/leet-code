package com.leetcode.cosminci._2600

object _2545_SortStudentsByKthScore:

  def sortTheStudents(score: Array[Array[Int]], k: Int): Array[Array[Int]] =
    score.sortBy(scores => -scores(k))
