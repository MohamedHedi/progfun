package recfun

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(194); 


 /**
* Exercise 1
*/
  def pascal(c: Int, r: Int): Int = c match {
      case 0 => 1
      case c if c >=r => 1
      case _ => pascal(c-1,r-1)+pascal(c,r-1)
  };System.out.println("""pascal: (c: Int, r: Int)Int""");$skip(554); 


  /**
* Exercise 2
*/
  def balance(chars: List[Char]): Boolean = {

    def process(chars: List[Char], myStack: Stack[Char]): Boolean = if (chars.isEmpty) myStack.isEmpty

      else {
        chars.head match {
          case '(' => process(chars.tail, myStack.push(chars.head))
          case ')' => if (myStack.contains('(')) process(chars.tail, myStack.pop)
                       else false
          case _ => process(chars.tail, myStack)
        }
      }

    val balancingAuxStack = new Stack[Char]

    process(chars, balancingAuxStack)
  };System.out.println("""balance: (chars: List[Char])Boolean""");$skip(289); 



  /**
* Exercise 3
*/
  def countChange(money: Int, coins: List[Int]): Int = {

    money match {
      case 0 => 1
      case x if x < 0 => 0
      case x if x>=1 && coins.isEmpty => 0
      case _ => countChange(money, coins.tail) + countChange(money - coins.head, coins)

    }

  };System.out.println("""countChange: (money: Int, coins: List[Int])Int""")}



}
