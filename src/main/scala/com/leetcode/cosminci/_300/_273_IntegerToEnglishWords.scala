package com.leetcode.cosminci._300

object _273_IntegerToEnglishWords:

  def numberToWords(num: Int): String =
    val to13 = "One Two Three Four Five Six Seven Eight Nine Ten Eleven Twelve Thirteen"
    val to19 = (to13 ++ " Fourteen Fifteen Sixteen Seventeen Eighteen Nineteen").split(' ')
    val tens = "Twenty Thirty Forty Fifty Sixty Seventy Eighty Ninety".split(' ')

    def words(n: Int): Seq[String] =
      if n < 20 then to19.slice(n - 1, n)
      else if n < 100 then tens(n / 10 - 2) +: words(n % 10)
      else if n < 1000 then to19(n / 100 - 1) +: "Hundred" +: words(n % 100)
      else Seq("Thousand", "Million", "Billion").zip(1 to 3)
        .collectFirst {
          case (w, p) if n < math.pow(1000, p + 1) =>
            words(n / math.pow(1000, p).toInt).appended(w) ++ words(n % math.pow(1000, p).toInt)
        }.get

    if words(num).isEmpty then "Zero" else words(num).mkString(" ")
