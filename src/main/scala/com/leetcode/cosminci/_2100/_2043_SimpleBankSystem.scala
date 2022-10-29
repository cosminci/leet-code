package com.leetcode.cosminci._2100

object _2043_SimpleBankSystem:

  class Bank(balance: Array[Long]):

    def transfer(account1: Int, account2: Int, money: Long): Boolean =
      if !withdraw(account1, money) then false
      else if deposit(account2, money) then true
      else {
        deposit(account1, money)
        false
      }

    def deposit(account: Int, money: Long): Boolean =
      if account > balance.length then false
      else {
        balance(account - 1) += money
        true
      }

    def withdraw(account: Int, money: Long): Boolean =
      if account > balance.length then false
      else if balance(account - 1) < money then false
      else {
        balance(account - 1) -= money
        true
      }
