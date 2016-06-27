package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(count: Int, min: Int, truncChars: List[Char]): (Int, Int) = {
      if (truncChars.isEmpty)
        (count, min)
      else {
        val newcount = truncChars.head match {
          case '(' => count + 1
          case ')' => count - 1
          case _ => count
        }
        loop(newcount, Math.min(newcount, min), truncChars.tail)
      }
    }
    val (count, min) = loop(0, 0, chars)
    if (count == 0 && min == 0) true else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def loop(currentAmount: Int, currentCoins: List[Int]): Int = {

      if (currentAmount == 0)
        1
      else if (currentCoins.isEmpty || currentAmount < 0)
        0
      else {
        loop(currentAmount, currentCoins.tail) + loop(currentAmount - currentCoins.head, currentCoins)
      }
    }
    loop(money, coins)
  }
}
