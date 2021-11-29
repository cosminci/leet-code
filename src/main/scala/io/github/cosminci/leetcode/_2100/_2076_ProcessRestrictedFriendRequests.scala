package io.github.cosminci.leetcode._2100

import io.github.cosminci.utils.UnionFind

object _2076_ProcessRestrictedFriendRequests:
  def friendRequests(n: Int, restrictions: Array[Array[Int]], requests: Array[Array[Int]]): Array[Boolean] =
    val uf = new UnionFind[Int]
    requests.map { case friends @ Array(f1, f2) =>
      val parents   = friends.map(uf.find).toSet
      val canAccept = restrictions.forall(_.map(uf.find).toSet != parents)
      Option.when(canAccept)(uf.union(f1, f2)).map(_ => true).getOrElse(false)
    }
