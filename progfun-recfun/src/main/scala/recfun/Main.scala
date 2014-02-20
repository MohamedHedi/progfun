package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = c match {
    case 0 => 1
    case c if c >= r => 1
    case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(acc: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) acc == 0
      else acc <= 0 && loop(valuate(acc, chars.head), chars.tail)
    }

    def valuate(count: Int, char: Char): Int = char match {
      case '(' => count - 1
      case ')' => count + 1
      case _ => count
    }

    loop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    money match {
      case 0 => 1
      case x if x < 0 => 0
      case x if x >= 1 && coins.isEmpty => 0
      case _ => countChange(money, coins.tail) + countChange(money - coins.head, coins)

    }

  }
}
