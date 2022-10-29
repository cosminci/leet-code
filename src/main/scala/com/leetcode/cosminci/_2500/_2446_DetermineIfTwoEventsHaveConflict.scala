package com.leetcode.cosminci._2500

import scala.util.chaining.*

object _2446_DetermineIfTwoEventsHaveConflict:

  def haveConflict(event1: Array[String], event2: Array[String]): Boolean =
    event1.head <= event2.last && event2.head <= event1.last
