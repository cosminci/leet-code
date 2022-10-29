package com.leetcode.cosminci._1000

import scala.collection.mutable

object _929_UniqueEmailAddresses:
  def main(args: Array[String]): Unit =
    println(
      numUniqueEmails(
        Array("test.email+alex@leetcode.com", "test.e.mail+bob.cathy@leetcode.com", "testemail+david@lee.tcode.com")
      )
    )

  def numUniqueEmails(emails: Array[String]): Int =
    emails
      .foldLeft(Set.empty[String]) {
        case (uniqueEmails, email) =>
          val (local, domain) = email.splitAt(email.indexOf('@'))
          val sanitizedLocal = local.split('+').head.filter(_ != '.')
          uniqueEmails + s"$sanitizedLocal$domain"
      }
      .size
