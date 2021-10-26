package io.github.cosminci.leetcode._800

import scala.collection.mutable

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

    println(
      accountsMerge(
        List(
          List("John", "johnsmith@mail.com", "john_newyork@mail.com"),
          List("John", "johnsmith@mail.com", "john00@mail.com"),
          List("Mary", "mary@mail.com"),
          List("John", "johnnybravo@mail.com")
        )
      )
    )

  private def accountsMerge(accounts: List[List[String]]): List[List[String]] =
    val accountEmailSets = mutable.Map.empty[String, mutable.Set[String]]
    val emailToAccount   = mutable.Map.empty[String, String]
    val accountToName    = mutable.Map.empty[String, String]

    accounts.foreach { acc =>
      val friendlyName = acc.head
      val emails       = acc.tail

      emails.find(emailToAccount.contains) match
        case None =>
          val primaryEmail = emails.head
          accountEmailSets.update(primaryEmail, mutable.Set.from(emails))
          emails.foreach { email => emailToAccount.update(email, emails.head) }
          accountToName.update(primaryEmail, friendlyName)

        case Some(existingEmail) =>
          val existingPrimaryEmail = emailToAccount(existingEmail)
          emails.foreach { email =>
            emailToAccount.get(email) match
              case None =>
                accountEmailSets(existingPrimaryEmail).add(email)
                emailToAccount.update(email, existingPrimaryEmail)

              case Some(primaryEmail) =>
                if primaryEmail != existingPrimaryEmail then
                  val emailsToMerge = accountEmailSets(primaryEmail)
                  emailsToMerge.foreach(emailToAccount.update(_, existingPrimaryEmail))
                  accountEmailSets(existingPrimaryEmail).addAll(emailsToMerge)
                  accountEmailSets.remove(primaryEmail)
                  accountToName.remove(primaryEmail)
          }
    }

    accountEmailSets.map { case (primaryEmail, emails) =>
      emails.toList.sorted.prepended(accountToName(primaryEmail))
    }.toList
