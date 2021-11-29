package io.github.cosminci.leetcode._800

import io.github.cosminci.utils.UnionFind

object _721_AccountsMerge:
  def main(args: Array[String]): Unit =
    accountsMerge(
      List(
        List("Alex", "Alex5@m.co", "Alex4@m.co", "Alex0@m.co"),
        List("Ethan", "Ethan3@m.co", "Ethan3@m.co", "Ethan0@m.co"),
        List("Kevin", "Kevin4@m.co", "Kevin2@m.co", "Kevin2@m.co"),
        List("Gabe", "Gabe0@m.co", "Gabe3@m.co", "Gabe2@m.co"),
        List("Gabe", "Gabe3@m.co", "Gabe4@m.co", "Gabe2@m.co")
      )
    ).foreach(println)

    accountsMerge(
      List(
        List("John", "johnsmith@mail.com", "john_newyork@mail.com"),
        List("John", "johnsmith@mail.com", "john00@mail.com"),
        List("Mary", "mary@mail.com"),
        List("John", "johnnybravo@mail.com")
      )
    ).foreach(println)

  def accountsMerge(accounts: List[List[String]]): List[List[String]] =
    val uf = new UnionFind[String]

    val emailToName = accounts.foldLeft(Map.empty[String, String]) {
      case (emailToName, account) =>
        val name :: emails = account
        emailToName.updated(emails.reduce(uf.union), name)
    }

    accounts
      .flatMap(_.tail)
      .distinct
      .groupBy(uf.find)
      .map { case (rootEmail, emails) => emailToName(rootEmail) +: emails.sorted}
      .toList
