package io.github.cosminci.leetcode._2100

import io.github.cosminci.utils.DisjointSetUnion.DSU

object _2076_ProcessRestrictedFriendRequests:
  def friendRequests(n: Int, restrictions: Array[Array[Int]], requests: Array[Array[Int]]): Array[Boolean] =
    val dsu = new DSU
    requests.map { case friends @ Array(f1, f2) =>
      val parents   = friends.map(dsu.find).toSet
      val canAccept = restrictions.forall(_.map(dsu.find).toSet != parents)
      Option.when(canAccept)(dsu.union(f1, f2)).map(_ => true).getOrElse(false)
    }
