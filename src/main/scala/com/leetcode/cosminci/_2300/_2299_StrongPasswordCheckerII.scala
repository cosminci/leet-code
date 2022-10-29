package com.leetcode.cosminci._2300

object _2299_StrongPasswordCheckerII:
  def strongPasswordCheckerII(password: String): Boolean =
    password.length >= 8 &&
      password.exists(_.isLower) &&
      password.exists(_.isUpper) &&
      password.exists(_.isDigit) &&
      password.intersect("!@#$%^&*()-+").nonEmpty &&
      password.sliding(2).forall(_.distinct.length > 1)
